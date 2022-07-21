{-# LANGUAGE TupleSections #-}
module Core.Compiler.Compiler where
  import Core.Compiler.Type
  import qualified Data.Map as M
  import Data.List (intercalate, union)
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.AST
  import Core.Compiler.CodeGen (CppAST(..))
  
  createTypeMap :: Type -> [String]
  createTypeMap Int = []
  createTypeMap Bool  = []
  createTypeMap Char  = []
  createTypeMap String  = []
  createTypeMap Float  = []
  createTypeMap (t1 :-> t2) =  t1' `union` createTypeMap t2
    where t1' = foldl union [] $ map createTypeMap t1
  createTypeMap (TVar x) = ["A" ++ show x]
  createTypeMap (ListT t) = createTypeMap t
  createTypeMap (TRec t) = foldl union [] m
    where f = map snd t
          m = map createTypeMap f
  createTypeMap (TApp n t) = foldl union [] $ map createTypeMap t
  createTypeMap (RefT t) = createTypeMap t

  fromType :: Type -> String
  fromType Int = "int"
  fromType Bool  = "bool"
  fromType Char  = "char"
  fromType String  = "char*"
  fromType Float  = "float"
  fromType (TVar x) = "A" ++ show x
  fromType (ListT t) = fromType t ++ "*"
  fromType (TRec t) = "struct {" ++  f ++ "}"
    where f = intercalate ", " $ map (\(x,t) -> x ++ ": " ++ fromType t) t
  fromType (TApp n t) = n ++ "<" ++ intercalate "," (map fromType t) ++ ">"
  fromType (t1 :-> t2) = "std::function<" ++ fromType t2 ++ "(" ++ intercalate "," (map fromType t1) ++ ")" ++ ">"
  fromType (RefT t) = fromType t ++ "*"

  createTemplate :: Type -> String
  createTemplate t = "template<" ++ intercalate "," m ++ ">"
    where m = createTypeMap t

  compileStatement :: MonadCompiler m => TypedStatement -> m [CppAST]
  compileStatement (Assignment (name :@ ty) e) = do
    e' <- compileExpression e
    return $ [CTemplate (createTypeMap ty) $ CDeclaration (name, fromType ty) e']
  compileStatement (Modified n e) = do
    n' <- compileExpression n
    e' <- compileExpression e
    return $ [CModification n' e']
  compileStatement (If e s1 s2) = do
    e' <- compileExpression e
    s1' <- compileStatement s1
    s2' <- compileStatement s2
    return $ [CIf e' (CSequence s1') (CSequence s2')]
  compileStatement (Sequence stmts) = do
    stmts' <- mapM compileStatement stmts
    return $ concat stmts'
  compileStatement (Expression e) 
    = (:[]) <$> compileExpression e
  compileStatement (Return e)
    = (:[]) . CReturn <$> compileExpression e
  compileStatement (Enum (n, ty) fields) = undefined

  compileExpression :: MonadCompiler m => TypedExpression -> m CppAST
  compileExpression _ = undefined