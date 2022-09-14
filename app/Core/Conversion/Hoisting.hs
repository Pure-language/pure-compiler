{-# LANGUAGE TupleSections #-}
module Core.Conversion.Hoisting where
  import Control.Monad.RWS
  import Core.TypeChecking.Type.AST
  import Control.Arrow (Arrow(second))
  
  type MonadHoist m = (MonadRWS () [TypedStatement] Int m)

  freshLambda :: MonadHoist m => m String
  freshLambda = do
    i <- get
    put (i + 1)
    return $ "lambda" ++ show i

  hoistStmt :: MonadHoist m => TypedStatement -> m TypedStatement
  hoistStmt (Assignment n e) = Assignment n <$> hoistExpr e
  hoistStmt (Return e) = Return <$> hoistExpr e
  hoistStmt (If e s1 s2) = If <$> hoistExpr e <*> hoistStmt s1 <*> hoistStmt s2
  hoistStmt (Sequence s) = Sequence <$> mapM hoistStmt s
  hoistStmt (Expression e) = Expression <$> hoistExpr e
  hoistStmt (Modified n e) = Modified n <$> hoistExpr e
  hoistStmt x = return x

  hoistExpr :: MonadHoist m => TypedExpression -> m TypedExpression
  hoistExpr (FunctionCall n es t) = do
    es <- mapM hoistExpr es
    return $ FunctionCall n es t
  hoistExpr (Lambda args body t) = do
    l <- freshLambda
    body' <- hoistStmt body
    tell [Assignment (l :@ t) (Lambda args body' t)]
    return (Variable l t)
  hoistExpr (BinaryOp op e1 e2) 
    = BinaryOp op <$> hoistExpr e1 <*> hoistExpr e2
  hoistExpr (UnaryOp op e) 
    = UnaryOp op <$> hoistExpr e
  hoistExpr (List exprs) 
    = List <$> mapM hoistExpr exprs
  hoistExpr (Index e i) 
    = Index <$> hoistExpr e <*> hoistExpr i
  hoistExpr (Structure fields s)
    = Structure <$> mapM (\(n, e) -> (n,) <$> hoistExpr e) fields <*> pure s
  hoistExpr (Object o f) 
    = Object <$> hoistExpr o <*> pure f
  hoistExpr (Ternary c t e) 
    = Ternary <$> hoistExpr c <*> hoistExpr t <*> hoistExpr e
  hoistExpr (LetIn n e1 e2 t) 
    = LetIn n <$> hoistExpr e1 <*> hoistExpr e2 <*> pure t
  hoistExpr (Reference e) 
    = Reference <$> hoistExpr e
  hoistExpr (Unreference e) 
    = Unreference <$> hoistExpr e
  hoistExpr (Match e cases) 
    = Match <$> hoistExpr e <*> mapM (\(x, y) -> (x,) <$> hoistStmt y) cases
  hoistExpr x = return x

  runHoisting :: Monad m => [TypedStatement] -> m [TypedStatement]
  runHoisting x = do
    (x, _, w) <- runRWST (mapM hoistStmt x) () 0
    return $ w ++ x