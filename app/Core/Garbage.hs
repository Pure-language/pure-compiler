{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Core.Garbage where
  import Control.Monad.State
  import Core.TypeChecking.Type.AST
  import Debug.Trace (traceShowM)
  
  type MonadGarbage m = (MonadState [String] m)

  runGarbageMonad :: Monad m => [TypedStatement] -> m [TypedStatement]
  runGarbageMonad x = do
    (x, env) <- runStateT (mapM runGarbage x) []
    return (filter (\case
      Assignment (n :@ _) _ -> notElem n env
      _ -> True) x)

  runGarbage :: MonadGarbage m => TypedStatement -> m TypedStatement
  runGarbage (Assignment (name :@ t) value) = do
    unless (name == "main") $ modify (name :)
    value' <- runGarbageExpr value
    return $ Assignment (name :@ t) value'
  runGarbage (Modified e v) = Modified <$> runGarbageExpr e <*> runGarbageExpr v
  runGarbage (If cond t f) = If <$> runGarbageExpr cond <*> runGarbageExpr t <*> runGarbageExpr f
  runGarbage (Expression e) = Expression <$> runGarbageExpr e
  runGarbage x = return x

  runGarbageCase :: MonadGarbage m => (TypedPattern, TypedExpression) -> m (TypedPattern, TypedExpression)
  runGarbageCase (p, s) = do
    s' <- runGarbageExpr s
    return (p, s')
  
  runGarbageExpr :: MonadGarbage m => TypedExpression -> m TypedExpression
  runGarbageExpr (Variable name t) = do
    modify $ filter (/= name)
    return $ Variable name t
  runGarbageExpr (FunctionCall n args t) = do
    n' <- runGarbageExpr n
    args' <- mapM runGarbageExpr args
    return $ FunctionCall n' args' t
  runGarbageExpr (Lambda args body t) = do
    body' <- runGarbageExpr body
    return $ Lambda args body' t
  runGarbageExpr (Constructor name t) = return $ Constructor name t
  runGarbageExpr (BinaryOp op e1 e2 t) = do
    e1' <- runGarbageExpr e1
    e2' <- runGarbageExpr e2
    return $ BinaryOp op e1' e2' t
  runGarbageExpr (UnaryOp op e t) = do
    e' <- runGarbageExpr e
    return $ UnaryOp op e' t
  runGarbageExpr (Throw e t) = do
    e' <- runGarbageExpr e
    return $ Throw e' t
  runGarbageExpr (List e t) = do
    e' <- mapM runGarbageExpr e
    return $ List e' t
  runGarbageExpr (Index e i t) = do
    e' <- runGarbageExpr e
    i' <- runGarbageExpr i
    return $ Index e' i' t
  runGarbageExpr (Structure n fields t) = do
    fields' <- mapM (\(n, e) -> (n,) <$> runGarbageExpr e) fields
    return $ Structure n fields' t
  runGarbageExpr (Object f p t) = do
    f' <- runGarbageExpr f
    return $ Object f' p t
  runGarbageExpr (Ternary cond t f t') = do
    cond' <- runGarbageExpr cond
    t'' <- runGarbageExpr t
    f'' <- runGarbageExpr f
    return $ Ternary cond' t'' f'' t'
  runGarbageExpr (LetIn name e body t) = do
    e' <- runGarbageExpr e
    body' <- runGarbageExpr body
    return $ LetIn name e' body' t
  runGarbageExpr (Reference name t) 
    = Reference <$> runGarbageExpr name <*> pure t
  runGarbageExpr (Unreference name t)
    = Unreference <$> runGarbageExpr name <*> pure t
  runGarbageExpr (Sequence stmts) = do
    stmts' <- mapM runGarbage stmts
    env <- get
    return $ Sequence (filter (\case
      Assignment (n :@ _) _ -> notElem n env
      _ -> True) stmts')
  runGarbageExpr (Match e cases) = Match <$> runGarbageExpr e <*> mapM runGarbageCase cases
  runGarbageExpr (Literal l t) = return $ Literal l t