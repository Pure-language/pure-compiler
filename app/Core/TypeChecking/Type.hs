{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Core.TypeChecking.Type where
  import Control.Monad.RWS
  import Control.Monad.Except
  import Core.Parser.AST
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.Methods (compose, union, applyEnv)
  import qualified Data.Set as S
  import qualified Data.Map as M
  import Core.TypeChecking.Substitution (Types(free, apply), Substitution)
  import Data.Foldable (foldlM)
  import Core.TypeChecking.Unification (mgu, check)
  import Text.Parsec (SourcePos)
  import Data.Either (isLeft)
  import Data.Maybe (isNothing)
  import Control.Arrow (Arrow(second, first))
  import qualified Core.TypeChecking.Type.AST as A
  import Data.List (unzip4)
  import Debug.Trace (traceShow)
  import Data.Bifunctor (Bifunctor(bimap))

  getPosition :: Located a -> (SourcePos, SourcePos)
  getPosition (_ :> pos) = pos

  type MonadType m = (MonadRWS Env () Int m, MonadIO m, MonadError (String, Maybe String, (SourcePos, SourcePos)) m)

  generalize :: Env -> Type -> Scheme
  generalize (env, cons) t = Forall (S.toList vars) t
    where vars = free t S.\\ (free env `S.union` free cons)

  -- Creating a new fresh type variable
  fresh :: MonadType m => m Type
  fresh = get >>= \n -> modify (+1) >> return (TVar n)

  -- Create a type instance based on a scheme
  tyInstantiate :: MonadType m => Scheme -> m Type
  tyInstantiate (Forall vars t) = do
    vars' <- mapM (const fresh) vars
    let s = M.fromList $ zip vars vars'
      in return $ apply s t

  {- TYPE DECLARATION PARSING -}

  getGenerics :: Declaration -> M.Map String Int -> Int -> (Int, M.Map String Int)
  getGenerics (Generic a) m i = case M.lookup a m of
    Just i' -> (i, m)
    Nothing -> (i + 1, M.insert a i m)
  getGenerics (Arrow args t) m i = (i', M.union m' args')
    where (_, m') = getGenerics t m i'
          (i', args') = foldl (\(i, m) a -> let (i', t) = getGenerics a m i in (i', m `M.union` t)) (i, m) args
  getGenerics (Array t) m i = getGenerics t m i
  getGenerics (Ref t) m i = getGenerics t m i
  getGenerics (AppE n xs) m i = (i', xs')
    where (i', xs') = foldl (\(i, m) x -> let (i', t) = getGenerics x m i in (i', m `M.union` t)) (i, m) xs
  getGenerics _ m i = (i, m)

  convert :: Declaration -> M.Map String Int -> Type
  convert (Generic a) m = case M.lookup a m of
    Just t -> TVar t
    Nothing -> error "Type variable not found"
  convert (Arrow args t) m = map (`convert` m) args :-> convert t m
  convert (Array t) m = ListT (convert t m)
  convert (StructE f) m = TRec (map (second (`convert` m)) f)
  convert (Ref t) m = RefT (convert t m)
  convert IntE _ = Int
  convert CharE _ = Char
  convert (AppE n xs) m = TApp n (map (`convert` m) xs)
  convert StrE _ = ListT Char
  convert FloatE _ = Float

  replace :: Declaration -> M.Map String Type -> Type
  replace (Generic a) m = case M.lookup a m of
    Just t -> t
    Nothing -> error "Type variable not found"
  replace (Arrow args t) m = map (`replace` m) args :-> replace t m
  replace (Array t) m = ListT (replace t m)
  replace (StructE f) m = TRec (map (second (`replace` m)) f)
  replace IntE _ = Int
  replace CharE _ = Char
  replace StrE _ = ListT Char
  replace FloatE _ = Float
  replace (AppE n xs) m = TApp n (map (`replace` m) xs)
  replace (Ref t) m = RefT (replace t m)

  buildType :: Declaration -> Scheme
  buildType d = do
    let (i, m) = getGenerics d M.empty 0
        ty     = convert d m
      in Forall (M.elems m) ty

  {- TYPE CHECKING -}

  -- Type checking a statement
  tyStatement :: MonadType m => Located Statement -> m (Maybe Type, Substitution, Env, A.TypedStatement)
  tyStatement (Modified name value :> pos) = do
    env <- ask
    (t1, s1, env1, n1) <- tyExpression name
    (t2, s2, env2, n2) <- tyExpression value
    case mgu t1 (RefT t2) of
      Right s -> do
        let s3 = s `compose` s2 `compose` s1
        return (Nothing, s3, applyEnv  s3 $ env1 `union` env2 `union` env, A.Modified n1 n2)
      Left err -> throwError (err, Nothing, pos)
  tyStatement z@((Assignment (name :@ ty) value) :> pos) = do
    -- Fresh type for recursive definitions
    (env, cons) <- ask
    (tv, e) <- case ty of
      Just t -> do
        t' <- tyInstantiate $ buildType t
        return (t', M.singleton name (Forall [] t'))
      Nothing -> do
        tv <- fresh
        return (tv, M.singleton name (Forall [] tv))
    (t1, s1, e1, v1) <- local (first (`M.union` e)) $ tyExpression value

    case mgu (apply s1 t1) (apply s1 tv) of
      Left err -> throwError (err, Nothing, pos)
      Right s -> return ()

    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
      Just t' -> do
        t'' <- tyInstantiate t'
        case mgu t1 t'' of
          Right s -> do
            let r = apply s t''
            return (s `compose` s1, r)
          Left x -> throwError (x, Just "Check for declarations and check types", pos)
      Nothing -> return (s1, t1)
    let env'  = M.delete name env
        t'    = generalize (applyEnv s3 (env, cons)) (apply s3 t2)
        env'' = M.insert name t' env'
    return (Nothing, s3, (env'', cons), A.Assignment (name A.:@ apply s3 t2) v1)
  tyStatement (Sequence stmts :> pos) = do
    (t1, s1, e1, v1) <- foldlM (\(t, s, e, v) stmt -> do
      (t', s', e', v') <- local ((e `union`)) $ tyStatement stmt
      case check s s' of
        Right s'' -> return (t', s'', e' `union` e, v ++ [v'])
        Left err -> throwError (err, Nothing, pos)) (Nothing, M.empty, (M.empty, M.empty), []) stmts
    return (t1, s1, e1, A.Sequence v1)
  tyStatement (If cond thenStmt elseStmt :> pos) = do
    (t1, s1, e1, v1) <- tyExpression cond
    (t2, s2, e2, v2) <- tyStatement thenStmt
    (t3, s3, e3, v3) <- tyStatement elseStmt
    let s4 = s1 `compose` s2 `compose` s3
    case mgu <$> t2 <*> t3 of
      Just (Right s) -> do
        let s5 = s `compose` s4
        case mgu (apply s5 t1) Bool of
          Right _ -> return (apply s5 t2, s5, apply s5 $ e1 `union` e2 `union` e3, A.If v1 v2 v3)
          Left x -> throwError (
            x, Just "Expression should return a boolean type",
            getPosition cond)

      Just (Left x) ->
        throwError (x, Nothing, pos)

      Nothing -> case (t2, t3) of
        (Nothing, Nothing) -> throwError (
          "Branches do not return anything",
          Just "Try to add statements in each branch", pos)

        (Nothing, _) -> throwError (
          "Then branch does not return anything",
          Just "Try to add statements in each branch",
          getPosition thenStmt)

        (_, Nothing) -> throwError (
          "Else branch does not return anything",
          Just "Try to add statements in each branch",
          getPosition elseStmt)

        _ -> throwError (
          "Branches do not return anything",
          Just "Try to add statements in each branch", pos)
  tyStatement (Return expr :> pos) = do
    (t1, s1, e1, v1) <- tyExpression expr
    return (Just t1, s1, e1, A.Return v1)
  tyStatement (Expression expr :> pos) = do
    (t1, s1, e1, v1) <- tyExpression (expr :> pos)
    return (Just t1, s1, e1, A.Expression v1)
  tyStatement (Enum name values :> pos) = do
    (genericsTable, _) <- foldlM (\(m, i) (name, ty) -> case ty of
      Just t -> do
        let (i, x) = foldl (\(i, m) a -> let (i', t) = getGenerics a m i in (i', m `M.union` t)) (i, m) t
          in return (x `M.union` m, i)
      Nothing -> return (m, i)) (M.empty, 0) values
    -- Creating a fresh type foreach type of reunion
    generic <- mapM (const fresh) genericsTable
    -- Recreating a map mapping generic name to type
    let table = M.fromList (zip (M.keys genericsTable) (M.elems generic))
    liftIO $ print table
    let header = TApp name $ M.elems table
    let tvs = map (\(name, ty) -> case ty of
          Just t -> do
            let t' = map (`replace` table) t
              in (name A.:@ (t' :-> header))
          Nothing -> name A.:@ header) values
    env <- ask
    let cons = M.fromList $ map (\(x A.:@ ty) -> (x, generalize env ty)) tvs
    return (Nothing, M.empty, second (M.union cons) env, A.Enum (name, M.elems table) tvs)

  tyExpression :: MonadType m => Located Expression -> m (Type, Substitution, Env, A.TypedExpression)
  tyExpression (Variable name :> pos) = do
    (env, cons) <- ask
    case M.lookup name (env `M.union` cons) of
      Just t -> do
        t' <- tyInstantiate t
        return (t', M.empty, (M.empty, M.empty), A.Variable name)
      Nothing -> throwError (
        "Variable " ++ name ++ " is not defined",
        Just $ "Try to define a new variable " ++ name, pos)
  tyExpression (Index e i :> pos) = do
    (t1, s1, e1, v1) <- tyExpression e
    (t2, s2, e2, v2) <- tyExpression i
    case mgu (apply s1 t2) Int of
      Right s -> do
        let s3 = s `compose` s1 `compose` s2
        tv <- fresh
        case mgu (apply s3 t1) (ListT tv) of
          Right s4 -> do
            return (apply s4 tv, s4 `compose` s3, e1 `union` e2, A.Index v1 v2)
          Left x -> throwError (x, Just "Make sure you passed a list or a string", pos)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Lambda args body :> pos) = do
    -- Reunion of all generics tables of explicit types
    (genericsTable, _) <- foldlM (\(m, i) (name :@ ty) -> case ty of
      Just t -> do
        let (i, x) = getGenerics t m i
          in return (x `M.union` m, i)
      Nothing -> return (m, i)) (M.empty, 0) args
    -- Creating a fresh type foreach type of reunion
    generic <- mapM (const fresh) genericsTable
    -- Recreating a map mapping generic name to type
    let table = M.fromList (zip (M.keys genericsTable) (M.elems generic))

    -- Creating a new argument type list based on map
    tvs <- foldlM (\t (_ :@ ty) -> case ty of
      Just t' -> return $ t ++ [replace t' table]
      Nothing -> fresh >>= \t' -> return $ t ++ [t']) [] args

    (env, cons) <- ask
    let args' = map (\(x :@ _) -> x) args
    let env'  = foldl (flip M.delete) env args'
        env'' = env' `M.union` M.fromList (zipWith (\x t -> (x, Forall [] t)) args' tvs)
    (t1, s1, _, b) <- local (const (env'', cons)) $ tyStatement body
    case t1 of
      Just t -> do
        let argTy = apply s1 tvs
        return (argTy :-> t, s1, (env, cons), A.Lambda (zipWith (A.:@) args' argTy) b)
      Nothing -> throwError (
        "Lambda does not return anything",
        Just "Try to add expressions or statements in body", pos)
  tyExpression (z@(FunctionCall n xs) :> pos) = do
    tv <- fresh
    (t1, s1, e1, n1) <- tyExpression n
    e <- ask
    (t2, s2, e2, args) <- foldlM (\(t, s, e, a) x -> do
      (t', s', e', a') <- local (apply s) $ tyExpression x
      return (t ++ [t'], s `compose` s', e' `union` e, a ++ [a'])) ([], s1, e, []) xs
    case mgu (t2 :-> tv) (apply s2 t1) of
      Right s3 -> return (apply s3 tv, s3 `compose` s2 `compose` s1, bimap (apply s3) (apply s3) e, A.FunctionCall n1 args)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Literal l :> _) = do
    env <- ask
    (,M.empty, env, A.Literal $ compileLit l) <$> tyLiteral l
  tyExpression ((BinaryOp op e1 e2) :> pos) =
    tyExpression (FunctionCall (Variable op :> pos) [e1, e2] :> pos)
  tyExpression (List elems :> pos) = do
    ls <- mapM tyExpression elems
    env <- ask
    case ls of
      [] -> do
        t <- fresh
        return (ListT t, M.empty, env, A.List (map (\(_, _, _, e) -> e) ls))
      (x:xs) ->
        let (x',_,_,_) = x
            xs' = map (\(x, _, _,_) -> x) xs
          in case doesUnify x' xs' of
            Left err -> throwError (err, Just "All elements of a list should have same type", pos)
            Right s -> return (ListT x', s, env, A.List (map (\(_, _, _, e) -> e) ls))
    where
      doesUnify :: Type -> [Type] -> Either String Substitution
      doesUnify t [] = Right M.empty
      doesUnify t (x:xs) = case mgu t x of
        Right s -> compose s <$> doesUnify t xs
        Left err -> Left err
  tyExpression (Structure fields :> pos) = do
    (ts, s, e, f) <- unzip4 <$> mapM (\(n, e) -> do
      (t, s, env, f) <- tyExpression e
      return ((n, t), s, env, (n, f))) fields
    return (TRec ts, foldl compose M.empty s, foldl union (M.empty, M.empty) e, A.Structure f)
  tyExpression (Object var property :> pos) = do
    env <- ask
    (t, s1, e, v1) <- tyExpression var
    tv <- fresh
    let s2 = mgu t (TRec [(property, tv)])
    let s3 = compose <$> s2 <*> pure s1
    case s3 of
      Right s -> do
        return (apply s tv, s, apply s e, A.Object v1 property)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Ternary cond e1 e2 :> pos) = do
    (t1, s1, env1, c) <- tyExpression cond
    (t2, s2, env2, t) <- tyExpression e1
    (t3, s3, env3, e) <- tyExpression e2
    let s4 = s1 `compose` s2 `compose` s3
    case mgu t2 t3 of
      Right s -> do
        let s5 = s `compose` s4
        case mgu (apply s5 t1) Bool of
          Right _ -> return (apply s5 t2, s5, apply s5 $ env1 `union` env2 `union` env3, A.Ternary c t e)
          Left x -> throwError (
            x, Just "Expression should return a boolean type",
            getPosition cond)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Reference n :> pos) = do
    (t, s, e, n1) <- tyExpression n
    return (RefT t, s, e, A.Reference n1)
  tyExpression (Unreference n :> pos) = do
    ty <- fresh
    (t, s, e, n1) <- tyExpression n
    case mgu t (RefT ty) of
      Right s' -> do
        let s2 = s' `compose` s
        return (apply s2 ty, s2, apply s2 e, A.Unreference n1)
      Left x -> throwError (x, Nothing, pos)
  tyExpression x = error $ "No supported yet: " ++ show x

  tyLiteral :: MonadType m => Literal -> m Type
  tyLiteral (I _) = return Int
  tyLiteral (S _) = return $ ListT Char
  tyLiteral (F _) = return Float
  tyLiteral (C _) = return Char

  compileLit :: Literal -> A.Literal
  compileLit (I (x :> _)) = A.I x
  compileLit (S (x :> _)) = A.S x
  compileLit (F (x :> _)) = A.F x
  compileLit (C (x :> _)) = A.C x

  env :: TypeEnv
  env = M.fromList [
      ("print", Forall [0] $ [TVar 0] :-> Int),
      ("+", Forall [0] $ [TVar 0, TVar 0] :-> TVar 0),
      ("-", Forall [0] $ [TVar 0, TVar 0] :-> TVar 0),
      ("*", Forall [0] $ [TVar 0, TVar 0] :-> TVar 0),
      ("/", Forall [0] $ [TVar 0, TVar 0] :-> TVar 0),

      ("<", Forall [0] $ [TVar 0, TVar 0] :-> Bool),
      (">", Forall [0] $ [TVar 0, TVar 0] :-> Bool),
      ("<=", Forall [0] $ [TVar 0, TVar 0] :-> Bool),
      (">=", Forall [0] $ [TVar 0, TVar 0] :-> Bool),
      ("==", Forall [0] $ [TVar 0, TVar 0] :-> Bool),
      ("!=", Forall [0] $ [TVar 0, TVar 0] :-> Bool)
    ]

  runCheck :: MonadIO m => [Located Statement] -> m (Either (String, Maybe String, (SourcePos, SourcePos)) [A.TypedStatement])
  runCheck stmt =
    (snd <$>) <$> foldlM (\e x -> case e of
      Right (e, a) -> do
        x <- runExceptT $ runRWST (tyStatement x) e 0
        case x of
          Right ((_, _, e, a'), _, _) -> return (Right (e, a ++ [a']))
          Left err -> return (Left err)
      Left err -> return (Left err)) (Right ((env, M.empty), [])) stmt