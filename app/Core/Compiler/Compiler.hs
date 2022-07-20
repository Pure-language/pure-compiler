module Core.Compiler.Compiler where
  import Core.Compiler.Type
  import qualified Data.Map as M
  import Data.List (intercalate, union)
  import Core.TypeChecking.Type.Definition

  createTypeMap :: Type -> [String]
  createTypeMap Int = []
  createTypeMap Bool  = []
  createTypeMap Char  = []
  createTypeMap String  = []
  createTypeMap Float  = []
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
  fromType (RefT t) = fromType t ++ "*"

  