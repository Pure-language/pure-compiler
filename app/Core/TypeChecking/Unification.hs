{-# LANGUAGE LambdaCase #-}
module Core.TypeChecking.Unification where
  import Core.TypeChecking.Substitution (Substitution, Types (free, apply))
  import Core.TypeChecking.Type.Definition (Type(..))
  import qualified Data.Map as M
  import Core.TypeChecking.Type.Methods (compose)
  import Data.These (These(..))
  import Data.Semialign.Indexed (SemialignWithIndex(ialignWith))
  
  variable :: Int -> Type -> Either String Substitution
  variable n t
    | t == TVar n = Right M.empty
    | n `elem` free t = Left $ "Occurs check failed: " ++ show t
    | otherwise = Right $ M.singleton n t

  check :: Substitution  -> Substitution  -> Either String Substitution 
  check s1 s2 = foldl compose M.empty <$> m
    where m = sequence $ ialignWith (\i -> \case
                This a -> Right (M.singleton i (apply s1 a))
                That b -> Right (M.singleton i (apply s2 b))
                These a b -> mgu (apply s1 a) (apply s2 b)) s1 s2

  mgu :: Type -> Type -> Either String Substitution
  mgu (TVar i) t = variable i t
  mgu t (TVar i) = variable i t
  mgu a@(t1 :-> t2) b@(t3 :-> t4)
    = if length t1 /= length t3
        then Left $ show a ++ " has " ++ show (length t1) ++ " arguments whereas " ++ show b ++ " has " ++ show (length t3) ++ " arguments"
        else let s1 = foldl (\acc (t, t') -> case mgu t t' of
                  Right s -> compose <$> (acc >>= check s) <*> acc
                  Left s -> Left s) (Right M.empty) $ zip t1 t3
        in compose <$> s1 <*> mgu t2 t4
  mgu (ListT t) (ListT t') = mgu t t'
  mgu Int Int = Right M.empty
  mgu String String = Right M.empty
  mgu Bool Bool = Right M.empty
  mgu Char Char = Right M.empty
  mgu s1 s2 = Left $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2