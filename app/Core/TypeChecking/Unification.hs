{-# LANGUAGE LambdaCase #-}
module Core.TypeChecking.Unification where
  import Core.TypeChecking.Substitution (Substitution, Types (free, apply))
  import Core.TypeChecking.Type.Definition (Type(..), Class (IsIn))
  import qualified Data.Map as M
  import Core.TypeChecking.Type.Methods (compose)
  import Data.These (These(..))
  import Data.Semialign.Indexed (SemialignWithIndex(ialignWith))
  import Data.Align (Semialign(alignWith))
  import Data.List (delete, union)
  import Control.Monad (foldM)
  import Debug.Trace (traceShow)
  
  variable :: Int -> Type -> Either String Substitution
  variable n t
    | t == TVar n = Right M.empty
    | n `elem` free t = Left $ "Occurs check failed in " ++ show t ++ " with " ++ show (TVar n)
    | otherwise = Right $ M.singleton n t

  align :: (Eq b, Eq a) => [(a, b)] -> [(a, b)] -> [((a, b), (a, b))]
  align ((x, y):xs) ys = case lookup x ys of
    Nothing -> align xs ys
    Just t -> ((x, y), (x, t)) : align xs (delete (x, t) ys)
  align [] _ = []

  check :: Substitution  -> Substitution  -> Either String Substitution 
  check s1 s2 = foldl compose M.empty <$> m
    where m = sequence $ ialignWith (\i -> \case
                This a -> Right (M.singleton i (apply s1 a))
                That b -> Right (M.singleton i (apply s2 b))
                These a b -> mgu (apply s1 a) (apply s2 b)) s1 s2

  constraintCheck :: [Class] -> [Class] -> Either String Substitution
  constraintCheck cs1 cs2 = foldl compose M.empty <$> m
    where m = sequence $ alignWith (\case
                That a -> Right M.empty
                This b -> Right M.empty
                These a b -> mguClass a b) cs1 cs2

  mguClass :: Class -> Class -> Either String Substitution  
  mguClass (IsIn c t) (IsIn c' t')
    | c == c' = mguList t t'
    | otherwise = Left $ "Classes " ++ show c ++ " and " ++ show c' ++ " do not match"

  mguList :: [Type] -> [Type] -> Either String Substitution
  mguList t1 t2 = foldl (\acc (t, t') -> case mgu t t' of
    Right s -> compose <$> (acc >>= check s) <*> pure s
    Left s -> Left s) (Right M.empty) $ zip t1 t2

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
  mgu Bool Bool = Right M.empty
  mgu Char Char = Right M.empty
  mgu (RefT t) (RefT t') = mgu t t'
  mgu (RefT t) (ListT t') = mgu t t'
  mgu (ListT t) (RefT t') = mgu t t'
  mgu Void Void = Right M.empty
  mgu (ps1 :=> t1) (ps2 :=> t2) = 
    compose <$> constraintCheck ps1 ps2 <*> mgu t1 t2
  mgu t (_ :=> t2) = mgu t t2
  mgu (_ :=> t) t2 = mgu t t2
  mgu a@(TApp n xs) b@(TApp n' xs') = if n == n' && length xs == length xs'
    then foldl (\acc (t, t') -> case mgu t t' of
                  Right s -> compose <$> (acc >>= check s) <*> acc
                  Left s -> Left s) (Right M.empty) $ zip xs xs'
    else Left $ "Type mismatch: " ++ show a ++ " and " ++ show b
  mgu t1@(TRec fs1) t2@(TRec fs2) = 
    let f = align fs1 fs2 `union` align fs2 fs1
      in foldM (\s (x, y) -> do
        s' <- mgu (snd x) (snd y)
        return $ compose s s') M.empty f
  mgu (TId n) (TId n') = if n == n' then Right M.empty else Left $ "Type mismatch: " ++ show n ++ " and " ++ show n'
  mgu s1 s2 = Left $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2