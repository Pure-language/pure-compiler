{-# LANGUAGE FlexibleInstances #-}
module Core.TypeChecking.Type.Methods where
  import Core.TypeChecking.Substitution (Types(..), Substitution)
  import Core.TypeChecking.Type.Definition (Type(..), TypeEnv, Scheme (Forall), Env, Class (IsIn), ConsEnv)
  import qualified Data.Set as S
  import qualified Data.Map as M
  import Data.Bifunctor (second, first)
  import Core.TypeChecking.Type.AST
  
  compose :: Substitution -> Substitution -> Substitution
  compose s1 s2 = M.map (apply s1) s2 `M.union` s1

  instance Types Type where
    free (TVar i) = S.singleton i 
    free (t1 :-> t2) = free t1 `S.union` free t2
    free Int = S.empty
    free (TRec fs) = S.unions $ map (free . snd) fs
    free (RefT t) = free t
    free (TApp n xs) = free xs
    free (cs :=> t) = free t `S.union` free cs
    free _ = S.empty

    apply s (TVar i) = case M.lookup i s of
      Just t -> t
      Nothing -> TVar i
    apply s (t1 :-> t2) = apply s t1 :-> apply s t2
    apply s (TRec fs) = TRec $ map (second $ apply s) fs
    apply s (RefT t) = RefT (apply s t)
    apply s (TApp n xs) = TApp (apply s n) $ apply s xs
    apply s (cs :=> t) = apply s cs :=> apply s t
    apply _ s = s
  
  instance Types Class where
    free (IsIn _ t) = free t
    apply s (IsIn c t) = IsIn c (apply s t)
  
  instance Types a => Types [a] where
    free = foldr (S.union . free) S.empty
    apply s = map (apply s)

  instance Types TypeEnv where
    free = free . M.elems
    apply = M.map . apply

  instance Types Scheme where
    free (Forall _ v t) = free t S.\\ S.fromList v
    apply s (Forall p v t) = Forall p v (apply (foldr M.delete s v) t)
    
  instance Types a => Types (Maybe a) where
    free = maybe S.empty free
    apply s = fmap (apply s)

  instance Types TypedStatement where
    free _ = undefined
    apply s (Assignment v e) = Assignment (apply s v) (apply s e)
    apply s (Modified n e) = Modified (apply s n) (apply s e)
    apply s (If c t e) = If (apply s c) (apply s t) (apply s e)
    apply s (Expression e) = Expression (apply s e)
    apply s (Return e) = Return (apply s e)
    apply s (Enum n e) = Enum n (apply s e)
    apply s (Record n e) = Enum n (apply s e)
    apply s (Extern n r) = Extern n (apply s r)
    apply s (For (n, t) e b) = For (n, apply s t) (apply s e) (apply s b)
    apply s (While c b) = While (apply s c) (apply s b)
    apply _ x = x

  instance Types (Annoted a) where
    free _ = undefined
    apply s (e :@ t) = e :@ apply s t

  instance (Types a, Types b) => Types (a, b) where
    free (a, b) = free a `S.union` free b
    apply s (a, b) = (apply s a, apply s b)

  instance Types TypedExpression where
    free _ = undefined
    apply s (FunctionCall n xs t) = FunctionCall (apply s n) (apply s xs) (apply s t)
    apply s (Lambda args b t) = Lambda (apply s args) (apply s b) (apply s t)
    apply s (Variable n t) = Variable n (apply s t)
    apply s (Literal l t) = Literal l (apply s t)
    apply s (BinaryOp op e1 e2 t) = BinaryOp op (apply s e1) (apply s e2) (apply s t)
    apply s (UnaryOp op e t) = UnaryOp op (apply s e) (apply s t)
    apply s (List xs t) = List (apply s xs) (apply s t)
    apply s (Index e i t) = Index (apply s e) (apply s i) (apply s t)
    apply s (Structure n fs t) = Structure n (map (second $ apply s) fs) (apply s t)
    apply s (Object e p t) = Object (apply s e) p (apply s t)
    apply s (Ternary c t e ty) = Ternary (apply s c) (apply s t) (apply s e) (apply s ty)
    apply s (Reference e t) = Reference (apply s e) (apply s t)
    apply s (Constructor n t) = Constructor n (apply s t)
    apply s (Unreference e t) = Unreference (apply s e) (apply s t)
    apply s (LetIn v e b t) = LetIn (apply s v) (apply s e) (apply s b) (apply s t)
    apply s (Match e ps) = Match (apply s e) (apply s ps)
    apply s (Sequence ss) = Sequence (apply s ss)
    apply s (Throw e t) = Throw (apply s e) (apply s t)

  instance Types TypedPattern where
    free _ = undefined
    apply s (AppP n xs) = AppP n (apply s xs)
    apply s (VarP v t) = VarP v (apply s t)
    apply _ s = s

  applyEnv :: Types a => Substitution -> (a, b) -> (a, b)
  applyEnv s (a, b) = (apply s a, b)

  applyCons :: Types b => Substitution -> (a, b) -> (a, b)
  applyCons s (a, b) = (a, apply s b)

  applyTypes :: (TypeEnv -> TypeEnv) -> Env -> Env
  applyTypes f (ty, cons) = (f ty, cons)
  
  applyCons' :: (ConsEnv -> ConsEnv) -> Env -> Env
  applyCons' f (ty, cons) = (ty, f cons)

  union :: (Ord k1, Ord k2) => (M.Map k1 v1, M.Map k2 v2) -> (M.Map k1 v1, M.Map k2 v2) -> (M.Map k1 v1, M.Map k2 v2)
  union (m1, m2) (m3, m4) = (M.union m1 m3, M.union m2 m4)
