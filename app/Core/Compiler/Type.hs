module Core.Compiler.Type (module Type, createTemplate, createTypeMap, fromType) where
  import Core.Compiler.Type.Free as Type
  import Core.Compiler.Type.Monad as Type
  import Core.TypeChecking.Type.Definition (Type(..))
  import Data.List (union, intercalate)
  
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