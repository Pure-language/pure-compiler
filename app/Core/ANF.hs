{-# LANGUAGE TupleSections #-}
module Core.ANF where
  import Core.TypeChecking.Type.AST
  import Control.Monad.State
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.Pretty ()
  import Debug.Trace (traceShowM)
  import Control.Monad.RWS (MonadRWS, execRWST, evalRWST, local, MonadReader (ask))
  import Data.Bifunctor (Bifunctor(first, second))
  import Data.List (union, nub)

  type Block = TypedStatement
  type MonadANF m = (MonadRWS [(String, String)] () Int m)

  createSequence :: [TypedStatement] -> TypedExpression
  createSequence = Sequence

  freshName :: MonadANF m => m String
  freshName = do
    i <- get
    put (i+1)
    return $ "$a" ++ show i

  convertStmt :: MonadANF m => TypedStatement -> m ([(Annoted String, TypedExpression)], TypedStatement)
  convertStmt (Assignment n@(name :@ _) e) = do
    (lets, e') <- local (`union` [(name, name)]) $  convertExpr e
    return (lets, Assignment n e')
  convertStmt (If e s1 s2) = do
    (lets, e') <- convertExpr e
    (lets1, s1') <- convertExpr s1
    (lets2, s2') <- convertExpr s2
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
  convertStmt x = return ([], x)

  convertExpr :: MonadANF m => TypedExpression -> m ([(Annoted String, TypedExpression)], TypedExpression)
  convertExpr (FunctionCall n es t) = do
    (lets1, n') <- convertExpr n
    (lets2, es') <- first concat . unzip <$> mapM convertExpr es
    return (lets1 ++ lets2, FunctionCall n' es' t)
  convertExpr (Lambda args body t) = do
    (lets, body') <- convertExpr body
    return (lets, Lambda args body' t)
  convertExpr (Sequence xs) = do
    (lets, xs') <- unzip <$> mapM convertStmt xs
    let lets' = map createLets lets
    let xs = concatMap (\(lets, x) -> lets ++ [x]) (zip lets' xs')
    return ([], createSequence xs)
  convertExpr (BinaryOp op e1 e2 t) = do
    (lets1, e1') <- convertExpr e1
    (lets2, e2') <- convertExpr e2
    return (lets1 ++ lets2, BinaryOp op e1' e2' t)
  convertExpr (UnaryOp op e t) = do
    (lets, e') <- convertExpr e
    return (lets, UnaryOp op e' t)
  convertExpr (List exprs t) = do
    (lets, exprs') <- first concat . unzip <$> mapM convertExpr exprs
    return (lets, List exprs' t)
  convertExpr (Index e i t) = do
    (lets1, e') <- convertExpr e
    (lets2, i') <- convertExpr i
    return (lets1 ++ lets2, Index e' i' t)
  convertExpr (Structure n fields t) = do
    (lets, fields') <- first concat . unzip <$> mapM (\(x, i) -> do
      (lets, e) <- convertExpr i
      return (lets, (x, e))) fields
    return (lets, Structure n fields' t)
  convertExpr (Object o f t) = do
    (lets, o') <- convertExpr o
    return (lets, Object o' f t)
  convertExpr (Ternary c t e ty) = do
    (lets1, c') <- convertExpr c
    (lets2, t') <- convertExpr t
    (lets3, e') <- convertExpr e
    return (lets1 ++ lets2 ++ lets3, Ternary c' t' e' ty)
  convertExpr (LetIn (n :@ ty) e body t) = do
    name1 <- freshName
    (lets1, e') <- local (`union` [(n, name1)]) $ convertExpr e
    (lets2, body') <- local (`union` [(n, name1)]) $ convertExpr body
    return (lets1 ++ [(name1 :@ ty, e')] ++ lets2, body')
  convertExpr (Reference e t) = do
    (lets, e') <- convertExpr e
    return (lets, Reference e' t)
  convertExpr (Unreference e t) = do
    (lets, e') <- convertExpr e
    return (lets, Unreference e' t)
  convertExpr (Variable n t) = ask >>= \env -> case lookup n env of
    Just n' -> return ([], Variable n' t)
    Nothing -> return ([], Variable n t)
  convertExpr (Match e cases) = do
    (lets1, e') <- convertExpr e
    (lets2, cases') <- first concat . unzip <$> mapM (\(p, b) -> do
      (lets, b') <- convertExpr b
      return (lets, (p, b'))) cases
    return (lets1 ++ lets2, Match e' cases')
  convertExpr x = return ([], x)

  returnType :: Type -> Type
  returnType (_ :-> t) = t
  returnType _ = error "returnType: not a function"

  createLets :: [(Annoted String, TypedExpression)] -> [TypedStatement]
  createLets [] = []
  createLets ((n, e):xs) = Assignment n e : createLets xs

  runANF :: Monad m => [TypedStatement] -> m [TypedStatement]
  runANF stmts = do
    x <- unzip . fst <$> evalRWST (mapM convertStmt stmts) [] 0
    let lets = map createLets $ fst x
    return $ concatMap (\(lets, x) -> lets ++ [x]) (zip lets (snd x))
    