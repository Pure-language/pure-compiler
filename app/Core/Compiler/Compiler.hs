{-# LANGUAGE TupleSections #-}
module Core.Compiler.Compiler where
  import Core.Compiler.Type
  import qualified Data.Map as M
  import Data.List (intercalate, union)
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.AST
  import Core.Compiler.CodeGen (CppAST(..), CType)
  import Control.Monad.RWS (gets)
  import Control.Monad.State (StateT(runStateT), evalStateT)
  import Core.Compiler.Modules.ADT (createEnum, createStructure)
  import Core.Compiler.Modules.Pattern (compileCase)
  
  compileStatement :: MonadCompiler m => TypedStatement -> m [CppAST]
  compileStatement (Assignment (name :@ ty) e) = do
    e' <- compileExpression e
    return [CTemplate (createTypeMap ty) $ CDeclaration (name, fromType ty) e']
  compileStatement (Modified n e) = do
    n' <- compileExpression n
    e' <- compileExpression e
    return [CModification n' e']
  compileStatement (If e s1 s2) = do
    e' <- compileExpression e
    s1' <- compileStatement s1
    s2' <- compileStatement s2
    return [CIfElse e' (CSequence s1') (CSequence s2')]
  compileStatement (Sequence stmts) = do
    stmts' <- mapM compileStatement stmts
    return $ concat stmts'
  compileStatement (Expression e)
    = (:[]) <$> compileExpression e
  compileStatement (Return e)
    = (:[]) . CReturn <$> compileExpression e
  compileStatement z@(Enum (n, ty) fields) = (:[createEnum z]) <$> createStructure z

  compileExpression :: MonadCompiler m => TypedExpression -> m CppAST
  compileExpression (Variable name) = return $ CVariable name
  compileExpression (FunctionCall call args ty) = do
    call' <- compileExpression call
    args' <- mapM compileExpression args
    return $ CCall call' args' []
  compileExpression (Reference c) = CRef <$> compileExpression c
  compileExpression (Unreference c) = CDeref <$> compileExpression c
  compileExpression z@(Lambda args body) = do
    let env = free z
    let args' = map (\(x :@ t) -> (x, fromType t)) args
    body' <- compileStatement body
    return $ CLambda env args' $ CSequence body'
  compileExpression (Match x pats) = do
    x <- compileExpression x
    CSequence <$> mapM (\(p, b) -> do
      b <- compileStatement b
      p <- compileCase p
      return $ p x b) pats
  compileExpression (Constructor c) = do
    getEnv >>= \env -> case M.lookup c env of
      Just (e, i) -> return $ if i then CCall (CStructProp (CVariable e) c) [] [] else CStructProp (CVariable e) c
      _ -> error $ "Constructor " ++ c ++ " is not an enum"
  compileExpression x = error . show $ x

  match :: [Type] -> [Type] -> [CType]
  match (t:ts) (t':ts') = matchType t t' ++ match ts ts'
  match _ _ = []

  matchType :: Type -> Type -> [CType]
  matchType Int Int = ["int"]
  matchType Bool Bool = ["bool"]
  matchType Char Char = ["char"]
  matchType String String = ["char*"]
  matchType Float Float = ["float"]
  matchType (TVar u) t = [fromType t]
  matchType (t1 :-> t2) (t1' :-> t2') = t' `union` matchType t2 t2'
    where t' = foldl union [] $ zipWith matchType t1 t1'
  matchType (TApp _ t) (TApp _ t') = foldl union [] $ zipWith matchType t t'
  matchType (TRec t) (TRec t') = foldl union [] $ zipWith matchType (map snd t) (map snd t')
  matchType (RefT t) (RefT t') = matchType t t'
  matchType _ _ = []

  runCompiler :: Monad m => [TypedStatement] -> m CppAST
  runCompiler stmts =
    CSequence . concat <$> evalStateT (mapM compileStatement stmts) emptyState