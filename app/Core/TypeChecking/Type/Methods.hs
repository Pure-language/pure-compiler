{-# LANGUAGE FlexibleInstances #-}
module Core.TypeChecking.Type.Methods where
  import Core.TypeChecking.Substitution (Types(..), Substitution)
  import Core.TypeChecking.Type.Definition (Type(..), TypeEnv, Scheme (Forall))
  import qualified Data.Set as S
  import qualified Data.Map as M
  import Data.Bifunctor (second)
  
  compose :: Substitution -> Substitution -> Substitution
  compose s1 s2 = M.map (apply s1) s2 `M.union` s1

  instance Types Type where
    free (TVar i) = S.singleton i 
    free (t1 :-> t2) = free t1 `S.union` free t2
    free Int = S.empty
    free String = S.empty
    free (ListT t) = free t
    free (TRec fs) = S.unions $ map (free . snd) fs
    free _ = S.empty

    apply s (TVar i) = case M.lookup i s of
      Just t -> t
      Nothing -> TVar i
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2
    apply s (ListT t) = ListT $ apply s t
    apply s (TRec fs) = TRec $ map (second $ apply s) fs
    apply _ s = s
  
  instance Types a => Types [a] where
    free = foldr (S.union . free) S.empty
    apply s = map (apply s)

  instance Types TypeEnv where
    free = free . M.elems
    apply = M.map . apply

  instance Types Scheme where
    free (Forall v t) = free t S.\\ S.fromList v
    apply s (Forall v t) = Forall v (apply (foldr M.delete s v) t)
    
  instance Types a => Types (Maybe a) where
    free = maybe S.empty free
    apply s = fmap (apply s)