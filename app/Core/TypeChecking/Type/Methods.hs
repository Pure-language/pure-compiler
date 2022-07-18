{-# LANGUAGE FlexibleInstances #-}
module Core.TypeChecking.Type.Methods where
  import Core.TypeChecking.Substitution (Types(..), Substitution)
  import Core.TypeChecking.Type.Definition (Type(..), TypeEnv, Scheme (Forall))
  import qualified Data.Set as S
  import qualified Data.Map as M
  import Data.Bifunctor (second, first)
  import Core.TypeChecking.Type.AST (TypedExpression(..), TypedStatement(..), Annoted(..))
  
  compose :: Substitution -> Substitution -> Substitution
  compose s1 s2 = M.map (apply s1) s2 `M.union` s1

  instance Types Type where
    free (TVar i) = S.singleton i 
    free (t1 :-> t2) = free t1 `S.union` free t2
    free Int = S.empty
    free String = S.empty
    free (ListT t) = free t
    free (TRec fs) = S.unions $ map (free . snd) fs
    free (RefT t) = free t
    free (TApp n xs) = S.unions (map free xs)
    free _ = S.empty

    apply s (TVar i) = case M.lookup i s of
      Just t -> t
      Nothing -> TVar i
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2
    apply s (ListT t) = ListT $ apply s t
    apply s (TRec fs) = TRec $ map (second $ apply s) fs
    apply s (RefT t) = RefT (apply s t)
    apply s (TApp n xs) = TApp n $ map (apply s) xs
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

  instance Types TypedStatement where
    free _ = undefined
    apply s (Assignment v e) = Assignment (apply s v) (apply s e)
    apply s (Modified n e) = Modified (apply s n) (apply s e)
    apply s (If c t e) = If (apply s c) (apply s t) (apply s e)
    apply s (Sequence ss) = Sequence (apply s ss)
    apply s (Expression e) = Expression (apply s e)
    apply s (Return e) = Return (apply s e)
    apply s (Enum n e) = Enum n (apply s e)

  instance Types (Annoted a) where
    free _ = undefined
    apply s (e :@ t) = e :@ apply s t

  instance (Types a, Types b) => Types (a, b) where
    free (a, b) = free a `S.union` free b
    apply s (a, b) = (apply s a, apply s b)

  instance Types TypedExpression where
    free _ = undefined
    apply s (FunctionCall n xs) = FunctionCall (apply s n) (apply s xs) 
    apply s (Lambda args b) = Lambda (apply s args) (apply s b)
    apply _ (Variable s) = Variable s
    apply _ (Literal l) = Literal l
    apply s (BinaryOp op e1 e2) = BinaryOp op (apply s e1) (apply s e2)
    apply s (UnaryOp op e) = UnaryOp op (apply s e)
    apply s (List xs) = List (apply s xs)
    apply s (Index e i) = Index (apply s e) (apply s i)
    apply s (Structure fs) = Structure (map (second $ apply s) fs)
    apply s (Object e p) = Object (apply s e) p
    apply s (Ternary c t e) = Ternary (apply s c) (apply s t) (apply s e)
    apply s (Reference e) = Reference (apply s e)
    apply s (Unreference e) = Unreference (apply s e)

  applyEnv :: Types a => Substitution -> (a, b) -> (a, b)
  applyEnv s = first (apply s)

  applyCons :: Types b => Substitution -> (a, b) -> (a, b)
  applyCons s = second (apply s)

  union :: (Ord k1, Ord k2) => (M.Map k1 v1, M.Map k2 v2) -> (M.Map k1 v1, M.Map k2 v2) -> (M.Map k1 v1, M.Map k2 v2)
  union (m1, m2) (m3, m4) = (M.union m1 m3, M.union m2 m4)