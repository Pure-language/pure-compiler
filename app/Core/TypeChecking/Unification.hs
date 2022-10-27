{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Core.TypeChecking.Unification where
  import Control.Monad.RWS
  import Control.Monad.Except
  import Core.TypeChecking.Substitution (Substitution, Types (free, apply))
  import Core.TypeChecking.Type.Definition (Type(..), Class (IsIn), Env, Instances, ConsEnv, Scheme (Forall))
  import qualified Data.Map as M
  import Core.TypeChecking.Type.Methods (compose)
  import Data.These (These(..))
  import Data.Semialign.Indexed (SemialignWithIndex(ialignWith))
  import Data.Align (Semialign(alignWith))
  import Data.List (delete, union)
  import Control.Monad (foldM)
  import Debug.Trace (traceShow)
  import Text.Parsec (SourcePos)
  import Data.Foldable (foldlM)
  import Core.TypeChecking.Type.AST (TypedStatement)
  import Core.TypeChecking.Type.Pretty ()
  import Core.Parser.AST (Located, Expression)
  
  type ReaderEnv = (M.Map String Type, Env)
  data Module = Module String [TypedStatement]
    deriving Show
  data Macro = MacroItem {
      macroName :: String,
      macroArgs :: [String],
      macroBody :: Located Expression,
      macroIsPublic :: Bool
    } deriving Show
  data TypeState = TypeState {
      counter :: Int,
      instances :: Instances,
      classEnv :: ClassEnv,
      modules :: M.Map String Module,
      macros :: M.Map String Macro
    } deriving Show
  type Methods = [String]
  type ClassEnv = M.Map String (Bool, Methods)
  type MonadType m = (MonadRWS ReaderEnv () TypeState m, MonadIO m, MonadError (String, Maybe String, (SourcePos, SourcePos)) m, MonadFail m)
  
  variable :: Int -> Type -> Either String Substitution
  variable n t
    | t == TVar n = Right M.empty
    | n `elem` free t = Left $ "Occurs check failed in " ++ show t ++ " with " ++ show (TVar n)
    | otherwise = Right $ M.singleton n t

  -- Creating a array of correspondances between two arrays
  align :: (Eq b, Eq a, Eq c) => [(a, b)] -> [(a, c)] -> [((a, b), (a, c))]
  align ((x, y):xs) ys = case lookup x ys of
    Nothing -> align xs ys
    Just t -> ((x, y), (x, t)) : align xs (delete (x, t) ys)
  align [] _ = []

  check :: ConsEnv -> Substitution  -> Substitution  -> Either String Substitution 
  check e s1 s2 = foldl compose M.empty <$> m
    where m = sequence $ ialignWith (\i -> \case
                This a -> Right (M.singleton i (apply s1 a))
                That b -> Right (M.singleton i (apply s2 b))
                These a b -> mgu e (apply s1 a) (apply s2 b)) s1 s2

  constraintCheck :: ConsEnv -> [Class] -> [Class] -> Either String Substitution
  constraintCheck e cs1 cs2 = foldl compose M.empty <$> m
    where m = sequence $ alignWith (\case
                That a -> Right M.empty
                This b -> Right M.empty
                These a b -> mguClass e a b) cs1 cs2

  mguClass :: ConsEnv -> Class -> Class -> Either String Substitution  
  mguClass e (IsIn c t) (IsIn c' t')
    | c == c' = mguList e t t'
    | otherwise = Left $ "Classes " ++ show c ++ " and " ++ show c' ++ " do not match"

  mguList :: ConsEnv -> [Type] -> [Type] -> Either String Substitution
  mguList e t1 t2 = foldl (\acc (t, t') -> case mgu e t t' of
    Right s -> compose <$> (acc >>= check e s) <*> pure s
    Left s -> Left s) (Right M.empty) $ zip t1 t2

  mgu :: ConsEnv -> Type -> Type -> Either String Substitution
  mgu e (TVar i) t = variable i t
  mgu e t (TVar i) = variable i t
  mgu e a@(t1 :-> t2) b@(t3 :-> t4)
    = if length t1 /= length t3
        then Left $ show a ++ " has " ++ show (length t1) ++ " arguments whereas " ++ show b ++ " has " ++ show (length t3) ++ " arguments"
        else let s1 = foldl (\acc (t, t') -> case mgu e t t' of
                  Right s -> compose <$> (acc >>= check e s) <*> acc
                  Left s -> Left s) (Right M.empty) $ zip t1 t3
        in compose <$> s1 <*> mgu e t2 t4
  mgu e Int Int = Right M.empty
  mgu e Bool Bool = Right M.empty
  mgu e Char Char = Right M.empty
  mgu e (RefT t) (RefT t') = mgu e t t'
  mgu e Void Void = Right M.empty
  mgu e (TId n) (TRec fs) = case M.lookup n e of
    Nothing -> Left $ "Type " ++ show n ++ " is not defined"
    Just (Forall _ _ ([TRec fs'] :-> _)) ->
      if length (align fs fs') == length fs && length (align fs fs') == length fs'
        then Right M.empty
        else Left $ "Record " ++ show n ++ " does not match " ++ show (TRec fs')
    Just _ -> Left $ "Type " ++ show n ++ " is not a record"
  mgu e (TRec fs) (TId n) = case M.lookup n e of
    Nothing -> Left $ "Type " ++ show n ++ " is not defined"
    Just (Forall _ _ ([TRec fs'] :-> _)) ->
      if length (align fs fs') == length fs && length (align fs fs') == length fs'
        then Right M.empty
        else Left $ "Record " ++ show n ++ " does not match " ++ show (TRec fs')
    Just _ -> Left $ "Type " ++ show n ++ " is not a record"
  mgu e (ps1 :=> t1) (ps2 :=> t2) = 
    compose <$> constraintCheck e ps1 ps2 <*> mgu e t1 t2
  mgu e t (_ :=> t2) = mgu e t t2
  mgu e (_ :=> t) t2 = mgu e t t2
  mgu e a@(TApp n xs) b@(TApp n' xs') = 
    compose <$> mgu e n n' <*> mgu e xs xs'
    -- if length xs == length xs'
    -- then let s = foldl (\acc (t, t') -> case mgu t t' of
    --               Right s -> compose <$> (acc >>= check s) <*> acc
    --               Left s -> Left s) (Right M.empty) $ zip xs xs'
    --       in compose <$> s <*> mgu n n'
    -- else Left $ "Type mismatch: " ++ show a ++ " and " ++ show b
  mgu e t1@(TRec fs1) t2@(TRec fs2) = 
    let f = align fs1 fs2 `union` align fs2 fs1
      in foldM (\s (x, y) -> do
        s' <- mgu e (snd x) (snd y)
        return $ compose s s') M.empty f
  mgu e (TId n) (TId n') = if n == n' then Right M.empty else Left $ "Type mismatch: " ++ show n ++ " and " ++ show n'
  mgu _ s1 s2 = Left $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2