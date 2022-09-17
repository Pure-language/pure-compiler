{-# LANGUAGE TupleSections #-}
module Core.Compiler.Compiler where
  import Core.Compiler.Type
  import qualified Data.Map as M
  import Data.List (intercalate, union)
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.AST
  import Core.Compiler.CodeGen (CppAST(..), CType, StructField (SType))
  import Control.Monad.RWS (gets, modify)
  import Control.Monad.State (StateT(runStateT), evalStateT)
  import Core.Compiler.Modules.ADT (createEnum, createStructure)
  import Core.Compiler.Modules.Pattern (compileCase)
  import Core.TypeChecking.Type.Pretty ()
  import Control.Arrow (Arrow(second))
  import Debug.Trace (traceShowM)

  compileStatement :: MonadCompiler m => TypedStatement -> m [CppAST]
  compileStatement (Assignment (name :@ ty) (Lambda args body t)) = do
    ret <- fromType $ case t of
      _ :-> ret -> ret
      _ -> error "Not a function"
    args <- mapM (\(x :@ t) -> (x,) <$> fromType t) args
    body' <- compileStatement body
    return [CFunction ret name args (if length body' == 1 then head body' else CSequence body')]
  compileStatement (Assignment (name :@ ty) e) = do
    e' <- compileExpression e
    ty' <- fromType ty
    return [CDeclaration (name, ty') e']
  compileStatement (Modified n e) = do
    n' <- compileExpression n
    e' <- compileExpression e
    return [CModification n' e']
  compileStatement (If e s1 s2) = do
    e' <- compileExpression e
    s1' <- compileStatement s1
    s2' <- compileStatement s2
    return [CIfElse e' (CSequence s1') (CSequence s2')]
  compileStatement (Record (name, ty) fields) = do
    modify $ \s -> s { structures = (name, ty) : structures s }
    fields' <- mapM (\(x :@ t) -> SType x <$> fromType t) fields
    return [CStruct name fields']
  compileStatement (Sequence stmts) = do
    stmts' <- mapM compileStatement stmts
    return $ concat stmts'
  compileStatement (Expression e)
    = (:[]) <$> compileExpression e
  compileStatement (Return e)
    = (:[]) . CReturn <$> compileExpression e
  compileStatement z@(Enum (n, ty) fields) = ([createEnum z]++) <$> createStructure z
  compileStatement (Extern n args ret) = do
    args' <- mapM fromType args
    ret' <- fromType ret
    return [CExtern n args' ret']
  compileStatement (Match x pats) = do
    x <- compileExpression x
    xs <- mapM (\(p, b) -> do
      b <- compileStatement b
      compileCase p x b) pats
    return $ [if length xs == 1 then head xs else foldl1 (\acc (CIf cond then') -> CIfElse cond then' acc) xs]

  compileExpression :: MonadCompiler m => TypedExpression -> m CppAST
  compileExpression (Variable name t) = return $ CVariable name
  --compileExpression (FunctionCall (Variable n) args ty) = do
  --  args' <- mapM compileExpression args
  --  ty' <- gets genericMap >>= \e -> return (case M.lookup n e of
  --    Just t -> match t ty
  --    Nothing -> [])
  --  return $ CCall (CVariable n) args' ty'
  compileExpression (FunctionCall call args ty) = do
    call' <- compileExpression call
    args' <- mapM compileExpression args
    return $ CCall call' args' []
  compileExpression (Reference c) = CRef <$> compileExpression c
  compileExpression (Unreference c) = CDeref <$> compileExpression c
  compileExpression (Constructor c t) = do
    getEnv >>= \env -> case M.lookup c env of
      Just (e, i) -> return $ if not i then CCall (CVariable c) [] [] else CVariable c
      _ -> error $ "Constructor " ++ c ++ " is not an enum"
  compileExpression (Structure fields t) = do
    fields' <- mapM (\(x, i) -> (x,) <$> compileExpression i) fields
    ty' <- fromType t
    return $ CCast ty' $ CLamStruct fields'
  compileExpression (Object obj f) = do
    obj' <- compileExpression obj
    return $ CStructProp obj' f
  compileExpression (Literal i) = return $ Lit i
  compileExpression (LetIn (name :@ ty) e (Variable "()" Void) t) = do
    e' <- compileExpression e
    ty' <- fromType ty
    return $ CDeclaration (name, ty') e'
  compileExpression x = error . show $ x



  --match :: [Type] -> [Type] -> [CType]
  --match (t:ts) (t':ts') = matchType t t' ++ match ts ts'
  --match _ _ = []

  --matchType :: Type -> Type -> [CType]
  --matchType Int Int = ["int"]
  --matchType Bool Bool = ["bool"]
  --matchType Char Char = ["char"]
  --matchType String String = ["char*"]
  --matchType Float Float = ["float"]
  --matchType (TVar u) t = [fromType t]
  --matchType (t1 :-> t2) (t1' :-> t2') = t' `union` matchType t2 t2'
  --  where t' = foldl union [] $ zipWith matchType t1 t1'
  --matchType (TApp _ t) (TApp _ t') = foldl union [] $ zipWith matchType t t'
  --matchType (TRec t) (TRec t') = foldl union [] $ zipWith matchType (map snd t) (map snd t')
  --matchType (RefT t) (RefT t') = matchType t t'
  --matchType _ _ = []

  runCompiler :: Monad m => [TypedStatement] -> m ([CppAST], CompilerState)
  runCompiler stmts = do
    (x, st) <- runStateT (mapM compileStatement stmts) emptyState
    return (concat x, st)