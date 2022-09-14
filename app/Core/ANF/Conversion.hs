{-# LANGUAGE TupleSections #-}
module Core.ANF.Conversion where
  import Core.TypeChecking.Type.AST
  import Control.Monad.State
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.Pretty ()
  import Control.Arrow (Arrow(second))

  type Block = TypedStatement
  type MonadANF m = MonadState (Int, [[Block]]) m

  addFrame :: MonadANF m => m ()
  addFrame = modify (second ([] :))

  addBlock :: MonadANF m => Block -> m ()
  addBlock b = do
    (i, bs) <- get
    put (i, (b : head bs) : tail bs)

  popFrame :: MonadANF m => m [Block]
  popFrame = do
    (i, bs) <- get
    put (i, tail bs)
    return $ head bs

  freshName :: MonadANF m => m String
  freshName = do
    (i, bs) <- get
    put (i+1, bs)
    return $ "%" ++ show i

  convertStmt :: MonadANF m => TypedStatement -> m TypedStatement
  convertStmt (Assignment name expr) = do
    expr' <- convertExpr expr
    addBlock $ Assignment name expr'
    return $ Assignment name expr'
  convertStmt (Modified name expr)
    = do
      expr' <- convertExpr expr
      addBlock $ Modified name expr'
      return $ Modified name expr'
  convertStmt (If cond thenBlock elseBlock) = do
    cond' <- convertExpr cond
    thenBlock' <- convertStmt thenBlock
    elseBlock' <- convertStmt elseBlock
    addBlock $ If cond' thenBlock' elseBlock'
    return $ If cond' thenBlock' elseBlock'
  convertStmt (Sequence stmts) = do
    addFrame
    mapM_ convertStmt stmts
    x <- Sequence . reverse <$> popFrame
    addBlock x
    return x
  convertStmt (Expression e) = do
    e' <- convertExpr e
    addBlock $ Expression e'
    return $ Expression e'
  convertStmt (Return e) = do
    e' <- convertExpr e
    addBlock $ Return e'
    return $ Return e'
  convertStmt x = do
    addBlock x
    return x

  returnType :: Type -> Type
  returnType (_ :-> t) = t
  returnType _ = error "returnType: not a function"

  convertExpr :: MonadANF m => TypedExpression -> m TypedExpression
  convertExpr (LetIn name expr body t) = do
    expr' <- convertExpr expr
    addBlock $ Assignment name expr'
    body' <- convertExpr body
    n <- freshName
    addBlock $ Assignment (n :@ t) body'
    return $ Variable n t
  convertExpr (Lambda args body t) = do
    addFrame
    body' <- convertStmt body
    blocks <- popFrame
    return $ Lambda args (Sequence $ reverse blocks) t
  convertExpr (FunctionCall name args t) = do
    args' <- mapM convertExpr args
    n <- freshName
    addBlock $ Assignment (n :@ returnType t) (FunctionCall name args' t)
    return $ Variable n t
  convertExpr (BinaryOp op e1 e2) = do
    e1' <- convertExpr e1
    e2' <- convertExpr e2
    return $ BinaryOp op e1' e2'
  convertExpr (UnaryOp op e) = do
    e' <- convertExpr e
    return $ UnaryOp op e'
  convertExpr (List exprs) = do
    exprs' <- mapM convertExpr exprs
    return $ List exprs'
  convertExpr (Index list index) = do
    list' <- convertExpr list
    index' <- convertExpr index
    return $ Index list' index'
  convertExpr (Structure fields t) = do
    fields' <- mapM (\(x, i) -> (x,) <$> convertExpr i) fields
    return $ Structure fields' t
  convertExpr (Object obj f) = do
    obj' <- convertExpr obj
    return $ Object obj' f
  convertExpr (Ternary c t e) = do
    c' <- convertExpr c
    t' <- convertExpr t
    e' <- convertExpr e
    return $ Ternary c' t' e'
  convertExpr (Reference e) = do
    e' <- convertExpr e
    return $ Reference e'
  convertExpr (Unreference e) = do
    e' <- convertExpr e
    return $ Unreference e'
  convertExpr (Match e cases) = do
    e' <- convertExpr e
    cases' <- mapM (\(p, b) -> (p,) <$> convertStmt b) cases
    return $ Match e' cases'
  convertExpr x = return x

  runANF :: Monad m => [TypedStatement] -> m [TypedStatement]
  runANF stmts =
    reverse . head . snd <$> execStateT (mapM convertStmt stmts) (0, [[]])