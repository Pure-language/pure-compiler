{-# LANGUAGE TupleSections #-}
module Core.Compiler.Type (module Type, createTemplate, createTypeMap, fromType) where
  import Core.Compiler.Type.Free as Type
  import Core.Compiler.Type.Monad as Type
  import Core.TypeChecking.Type.Definition (Type(..))
  import Data.List (union, intercalate, find)
  import Control.Monad.RWS (gets)
  import Data.Either (isRight)
  import Core.TypeChecking.Unification (mgu)

  createTypeMap :: Type -> [String]
  createTypeMap Int = []
  createTypeMap Bool  = []
  createTypeMap Char  = []
  createTypeMap Float  = []
  createTypeMap (t1 :-> t2) =  t1' `union` createTypeMap t2
    where t1' = foldl union [] $ map createTypeMap t1
  createTypeMap (TVar x) = ["A" ++ show x]
  createTypeMap (ListT t) = createTypeMap t
  createTypeMap (TRec t) = foldl union [] m
    where f = map snd t
          m = map createTypeMap f
  createTypeMap (TApp n t) = createTypeMap n `union` foldl union [] (map createTypeMap t)
  createTypeMap (RefT t) = createTypeMap t
  createTypeMap _ = []

  fromType :: MonadCompiler m => Type -> m String
  fromType Int = return "int"
  fromType Bool = return "bool"
  fromType Char = return "char"
  fromType Float = return "float"
  fromType (TVar x) = return "void*"
  fromType (ListT t) = (++"*") <$> fromType t
  fromType z@(TRec t) = do
    structs <- gets structures
    case find (\(x, t) -> isRight $ mgu t z) structs of
      Just (x, _) -> return $ "struct " ++  x
      Nothing -> do
        name <- addStructure z
        fields <- mapM (\(x, y) -> (x,) <$> fromType y) t
        addToplevel $ "struct " ++ name ++ " {" ++ concatMap (\(x, y) -> y ++ " " ++ x ++ ";") fields ++ "};"
        return $ "struct " ++ name
  fromType (TApp n t) = fromType n
  fromType (t1 :-> t2) = do
    -- typedef ret (*name)(args);
    name <- ("tLambda" ++) . show <$> inc
    args <- mapM fromType t1
    ret <- fromType t2
    addToplevel ("typedef " ++ ret ++ " (*" ++ name ++ ")(" ++ intercalate ", " args ++ ");")
    return name
  fromType (TId n) = gets structures >>= \structs -> case lookup n structs of
    Just _ -> return $ "struct " ++ n
    Nothing -> return n
  fromType (RefT t) = (++ "*") <$> fromType t
  fromType Void = return "void"

  createTemplate :: Type -> String
  createTemplate t = "template<" ++ intercalate "," m ++ ">"
    where m = createTypeMap t