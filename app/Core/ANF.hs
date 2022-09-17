{-# LANGUAGE TupleSections #-}
module Core.ANF where
  import Core.TypeChecking.Type.AST
  import Control.Monad.State
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.Pretty ()
  import Debug.Trace (traceShowM)
  import Control.Monad.RWS (MonadRWS, execRWST, evalRWST, local, MonadReader (ask))
  import Data.Bifunctor (Bifunctor(first, second))
  import Data.List (union)
  
  type Block = TypedStatement
  type MonadANF m = (MonadRWS [(String, String)] () Int m)

  createSequence :: [TypedStatement] -> TypedStatement
  createSequence x
    | length x == 1 = head x
    | otherwise = Sequence x

  freshName :: MonadANF m => m String
  freshName = do
    i <- get
    put (i+1)
    return $ "$a" ++ show i

  convertStmt :: MonadANF m => TypedStatement -> m ([(Annoted String, TypedExpression)], TypedStatement)
  convertStmt (Assignment n@(name :@ _) e) = do
    (lets, e') <- local (`union` [(name, name)]) $  convertExpr e
    return (lets, Assignment n e')
  convertStmt (Sequence xs) = do
    (lets, xs') <- first concat . unzip <$> mapM convertStmt xs
    return ([], createSequence $ createLets lets ++ xs')
  convertStmt (If e s1 s2) = do
    (lets, e') <- convertExpr e
    (lets1, s1') <- convertStmt s1
    (lets2, s2') <- convertStmt s2
    return (lets ++ lets1 ++ lets2, If e' s1' s2')
  convertStmt (Modified n e) = do
    (lets, e') <- convertExpr e
    return (lets, Modified n e')
  convertStmt (Expression e) = do
    (lets, e') <- convertExpr e
    return (lets, Expression e')
  convertStmt (Return e) = do
    (lets, e') <- convertExpr e
    return (lets, Return e')
  convertStmt (Match e cases) = do
    (lets1, e') <- convertExpr e
    (lets2, cases') <- first concat . unzip <$> mapM (\(p, b) -> do
      (lets, b') <- convertStmt b
      return (lets, (p, b'))) cases
    return (lets1 ++ lets2, Match e' cases')
  convertStmt x = return ([], x)

  convertExpr :: MonadANF m => TypedExpression -> m ([(Annoted String, TypedExpression)], TypedExpression)
  convertExpr (FunctionCall n es t) = do
    (lets1, n') <- convertExpr n
    (lets2, es') <- first concat . unzip <$> mapM convertExpr es
    return (lets1 ++ lets2, FunctionCall n' es' t)
  convertExpr (Lambda args body t) = do
    (lets, body') <- convertStmt body
    return (lets, Lambda args body' t)
  convertExpr (BinaryOp op e1 e2) = do
    (lets1, e1') <- convertExpr e1
    (lets2, e2') <- convertExpr e2
    return (lets1 ++ lets2, BinaryOp op e1' e2')
  convertExpr (UnaryOp op e) = do
    (lets, e') <- convertExpr e
    return (lets, UnaryOp op e')
  convertExpr (List exprs) = do
    (lets, exprs') <- first concat . unzip <$> mapM convertExpr exprs
    return (lets, List exprs')
  convertExpr (Index e i) = do
    (lets1, e') <- convertExpr e
    (lets2, i') <- convertExpr i
    return (lets1 ++ lets2, Index e' i')
  convertExpr (Structure fields t) = do
    (lets, fields') <- first concat . unzip <$> mapM (\(x, i) -> do
      (lets, e) <- convertExpr i
      return (lets, (x, e))) fields
    return (lets, Structure fields' t) 
  convertExpr (Object o f) = do
    (lets, o') <- convertExpr o
    return (lets, Object o' f)
  convertExpr (Ternary c t e) = do
    (lets1, c') <- convertExpr c
    (lets2, t') <- convertExpr t
    (lets3, e') <- convertExpr e
    return (lets1 ++ lets2 ++ lets3, Ternary c' t' e')
  convertExpr (LetIn (n :@ ty) e body t) = do
    name1 <- freshName
    (lets1, e') <- local (`union` [(n, name1)]) $ convertExpr e
    (lets2, body') <- local (`union` [(n, name1)]) $ convertExpr body
    traceShowM (n, body')
    return ([(name1 :@ ty, e')] ++ lets1 ++ lets2, body')
  convertExpr (Reference e) = do
    (lets, e') <- convertExpr e
    return (lets, Reference e')
  convertExpr (Unreference e) = do
    (lets, e') <- convertExpr e
    return (lets, Unreference e')
  convertExpr (Variable n t) = ask >>= \env -> case lookup n env of
    Just n' -> return ([], Variable n' t)
    Nothing -> return ([], Variable n t)
  convertExpr x = return ([], x)
  
  returnType :: Type -> Type
  returnType (_ :-> t) = t
  returnType _ = error "returnType: not a function"

  createLets :: [(Annoted String, TypedExpression)] -> [TypedStatement]
  createLets [] = []
  createLets ((n, e):xs) = Assignment n e : createLets xs

  runANF :: Monad m => [TypedStatement] -> m [TypedStatement]
  runANF stmts = do
    x <- first concat . unzip . fst <$> evalRWST (mapM convertStmt stmts) [] 0
    let lets = createLets $ fst x
    return $ lets ++ snd x
    