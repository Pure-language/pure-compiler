{-# LANGUAGE TupleSections #-}
module Core.Monomorphization where
  import Control.Monad.RWS
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.AST
  import Core.TypeChecking.Type.Pretty ()
  import Debug.Trace (traceShowM)
  import Core.TypeChecking.Unification (mgu)
  import Data.Map (toList)
  import Data.List (intercalate)
  import Core.TypeChecking.Substitution (Types(apply))

  type MonadMono m = MonadRWS () [TypedStatement] [(String, (Type, TypedExpression))] m

  addEnv :: MonadMono m => String -> (Type, TypedExpression) -> m ()
  addEnv name typ = do
    env <- get
    put ((name, typ) : env)

  monomorphizeStmt :: MonadMono m => TypedStatement -> m TypedStatement
  monomorphizeStmt (Assignment (name :@ ty) value) = do
    value' <- monomorphizeExpr value
    addEnv name (ty, value')
    return $ Assignment (name :@ ty) value'
  monomorphizeStmt (Modified name value) = do
    Modified name <$> monomorphizeExpr value
  monomorphizeStmt (If cond thenBranch elseBranch) = do
    cond' <- monomorphizeExpr cond
    thenBranch' <- monomorphizeStmt thenBranch
    elseBranch' <- monomorphizeStmt elseBranch
    return $ If cond' thenBranch' elseBranch'
  monomorphizeStmt (Sequence stmts) = do
    s <- get
    stmts' <- mapM monomorphizeStmt stmts
    put s
    return $ Sequence stmts'
  monomorphizeStmt (Return value) = do
    value' <- monomorphizeExpr value
    return $ Return value'
  monomorphizeStmt (Expression e) = do
    e' <- monomorphizeExpr e
    return $ Expression e'
  monomorphizeStmt (Match e cases) = do
    traceShowM e
    e' <- monomorphizeExpr e
    cases' <- mapM (\(x, i) -> (x,) <$> monomorphizeStmt i) cases
    return $ Match e' cases'
  monomorphizeStmt x = return x

  monomorphizeExpr :: MonadMono m => TypedExpression -> m TypedExpression
  --monomorphizeExpr (FunctionCall (Variable n t) args t') = --do
  --  args' <- mapM monomorphizeExpr args
  --  get >>= \env -> case lookup n env of
  --    Just t -> do
  --      traceShowM (n, t)
  --      return $ FunctionCall (Variable n t) args' t'
  --    _ -> 
  --      return $ FunctionCall (Variable n t) args' t'
  monomorphizeExpr (FunctionCall n args t) = do
    n' <- monomorphizeExpr n
    args' <- mapM monomorphizeExpr args
    return $ FunctionCall n' args' t
  monomorphizeExpr (Lambda args body t) = do
    body' <- monomorphizeStmt body
    return $ Lambda args body' t
  monomorphizeExpr (Variable n t) = do
    get >>= traceShowM
    get >>= \env -> case lookup n env of
      Just (t', b) -> case mgu t' t of
        Right s -> do
          if null s
            then return $ Variable n t
            else do
            let nameFromSub = map (showTy . snd) $ toList s
            let name = n ++ "_" ++ intercalate "_" nameFromSub
            tell [Assignment (name :@ t) (apply s b)]
            return (Variable name t)
        Left err -> error $ "monomorphization failed: " ++ err
      Nothing -> return $ Variable n t
  monomorphizeExpr (Literal l) = return $ Literal l
  monomorphizeExpr (BinaryOp op a b) = do
    a' <- monomorphizeExpr a
    b' <- monomorphizeExpr b
    return $ BinaryOp op a' b'
  monomorphizeExpr (UnaryOp op a) = do
    a' <- monomorphizeExpr a
    return $ UnaryOp op a'
  monomorphizeExpr (Constructor n t) = return $ Constructor n t
  monomorphizeExpr (List exprs) = do
    exprs' <- mapM monomorphizeExpr exprs
    return $ List exprs'
  monomorphizeExpr (Index list index) = do
    list' <- monomorphizeExpr list
    index' <- monomorphizeExpr index
    return $ Index list' index'
  monomorphizeExpr (Structure fields t) = do
    fields' <- mapM (\(x, i) -> (x,) <$> monomorphizeExpr i) fields
    return $ Structure fields' t
  monomorphizeExpr (Object o f) = do
    o' <- monomorphizeExpr o
    return $ Object o' f
  monomorphizeExpr (Ternary c t e) = do
    c' <- monomorphizeExpr c
    t' <- monomorphizeExpr t
    e' <- monomorphizeExpr e
    return $ Ternary c' t' e'
  monomorphizeExpr (LetIn (name :@ ty) v e t) = do
    traceShowM (name, ty)
    s <- get
    v' <- monomorphizeExpr v
    addEnv name (ty, v')
    e' <- monomorphizeExpr e
    put s
    return $ LetIn (name :@ ty) v' e' t
  monomorphizeExpr (Reference e) = do
    e' <- monomorphizeExpr e
    return $ Reference e'
  monomorphizeExpr (Unreference e) = do
    e' <- monomorphizeExpr e
    return $ Unreference e'

  showTy :: Type -> String
  showTy (TVar i) = "t" ++ show i
  showTy (t :-> u) = "fun_" ++ intercalate "_" (map show t) ++ "_" ++ show u
  showTy (ListT Char) = "String"
  showTy Int = "Int"
  showTy Void = "Void"
  showTy Float = "Float"
  showTy Bool = "Bool"
  showTy Char = "Char"
  showTy (ListT t) = "list_" ++ showTy t
  showTy (TRec fs) = "struct_" ++ intercalate "_" (map (\(n, t) -> n ++ "-" ++ showTy t) fs)
  showTy (RefT t) = "ref_" ++ showTy t
  showTy (TId s) = s
  showTy (TApp s args) = showTy s ++ intercalate "_" (map show args)

  monomorphize :: Monad m => [TypedStatement] -> m [TypedStatement]
  monomorphize x = do
    (x, _, monos) <- runRWST (mapM monomorphizeStmt x) () []
    return (monos ++ x)