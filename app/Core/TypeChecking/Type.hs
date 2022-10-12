
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Core.TypeChecking.Type where
  import Control.Monad.RWS
  import Control.Monad.Except
  import Core.Parser.AST
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.Methods
  import qualified Data.Set as S
  import qualified Data.Map as M
  import Core.TypeChecking.Substitution (Types(free, apply), Substitution)
  import Data.Foldable (foldlM)
  import Core.TypeChecking.Unification (mgu, check, constraintCheck)
  import Text.Parsec (SourcePos)
  import Data.Either (isLeft, fromRight, isRight)
  import Data.Maybe (isNothing)
  import qualified Core.TypeChecking.Type.AST as A
  import Data.List (unzip4)
  import Debug.Trace (traceShow, traceShowM)
  import qualified Data.Bifunctor as B
  import qualified Data.List as L
  import Core.TypeChecking.Type.Pretty ()

  unannotate :: A.Annoted a -> (a, Type)
  unannotate (a A.:@ t) = (a, t)

  getPosition :: Located a -> (SourcePos, SourcePos)
  getPosition (_ :> pos) = pos

  type ReaderEnv = (M.Map String Type, Env)
  type MonadType m = (MonadRWS ReaderEnv () (Int, Instances) m, MonadIO m, MonadError (String, Maybe String, (SourcePos, SourcePos)) m, MonadFail m)

  generalize :: Env -> Type -> Scheme
  generalize (env, cons) t = Forall (S.toList vars) t
    where vars = free t S.\\ free env

  freshInstance :: MonadType m => Instance -> m ()
  freshInstance t = modify (B.second (t:))

  -- Creating a new fresh type variable
  fresh :: MonadType m => m Type
  fresh = gets fst >>= \n -> modify (B.first (+1)) >> return (TVar n)

  -- Create a type instance based on a scheme
  tyInstantiate :: MonadType m => Scheme -> m Type
  tyInstantiate (Forall vars t) = do
    vars' <- mapM (const fresh) vars
    let s = M.fromList $ zip vars vars'
      in return $ apply s t

  {- TYPE DECLARATION PARSING -}


  createType :: MonadType m => Declaration -> M.Map String Type -> m Type
  createType (Id name) env = case M.lookup name env of
    Just t -> return t
    Nothing -> return $ TId name
  createType (Arrow annot args ret) env = do
    newEnv <- M.fromList <$> mapM (\x -> (x,) <$> fresh) annot
    args' <- mapM (`createType` (newEnv `M.union` env)) args
    ret' <- createType ret (newEnv `M.union` env)
    return $ args' :-> ret'
  createType (Array t) env = do
    t' <- createType t env
    return $ ListT t'
  createType (StructE fields) env = do
    fields' <- mapM (\(name, t) -> (name,) <$> createType t env) fields
    return $ TRec fields'
  createType (AppE name args) env
    = TApp (TId name) <$> mapM (`createType` env) args
  createType StrE _ = return (ListT Char)
  createType VoidE _ = return Void
  createType IntE _ = return Int
  createType FloatE _ = return Float
  createType CharE _ = return Char
  createType (Ref t) env = RefT <$> createType t env

  replace :: Declaration -> M.Map String Type -> Type
  replace (Id a) m = case M.lookup a m of
    Just t -> t
    Nothing -> TId a
  replace (Arrow annot args t) m = map (`replace` m) args :-> replace t m
  replace (Array t) m = ListT (replace t m)
  replace (StructE f) m = TRec (map (B.second (`replace` m)) f)
  replace IntE _ = Int
  replace CharE _ = Char
  replace StrE _ = ListT Char
  replace FloatE _ = Float
  replace VoidE _ = Void
  replace (AppE n xs) m = TApp (TId n) (map (`replace` m) xs)
  replace (Ref t) m = RefT (replace t m)

  {- TYPE CHECKING -}

  instances :: MonadType m => m Instances
  instances = gets snd

  trimap :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
  trimap f g h (a, c, e) = (f a, g c, h e)

  find' :: MonadType m => (SourcePos, SourcePos) -> [Class] -> [([Class], (String, [Class]))] -> m ([A.TypedExpression], [(String, Type)], [Class])
  find' a subCls env = trimap concat concat concat . unzip3 <$> mapM
    (\x -> if containsTVar' (appify x)
        then return (map (\z@(IsIn cls tys) ->
          A.Variable (cls ++ createTypeInstName tys) (appify z)) subCls,
          map (\z@(IsIn cls tys) ->
            (cls ++ createTypeInstName tys, appify z)) subCls, [x])
        else case map (B.first $ fromRight M.empty) $ filter (isRight . fst) $ map (\z@(cls', _) -> (constraintCheck cls' [x], z)) env of
      -- If a superclass instance exists
      [(s, ([z@(IsIn cls t2)], (name, subCls')))] -> do
        (subVar, subTC, cls) <- find' a (apply s subCls') env
        --liftIO $ print s
        let var = A.Variable name (apply s (appify z))
          in return ([if null subVar
            then var
            else A.FunctionCall var subVar (apply s (appify z))], subTC, cls)

      xs -> if containsTVar' (appify x)
        then return (map (\z@(IsIn cls tys) ->
          A.Variable (cls ++ createTypeInstName tys) (appify z)) subCls,
          map (\z@(IsIn cls tys) ->
            (cls ++ createTypeInstName tys, appify z)) subCls, [x])
        else
          if null xs
            then throwError ("No instance found for " ++ show x, Nothing, a)
            else throwError ("Instances " ++ L.intercalate ", " (map (\(_, (x:xs, _)) -> show x) xs) ++ " overlaps for " ++ show x, Nothing, a)) subCls

  getTVars :: Type -> [Type]
  getTVars (TApp t xs) = getTVars t ++ concatMap getTVars xs
  getTVars (TVar x) = [TVar x]
  getTVars (t1 :-> t2) = concatMap getTVars t1 ++ getTVars t2
  getTVars (ListT t) = getTVars t
  getTVars (TRec xs) = concatMap (getTVars . snd) xs
  getTVars (RefT t) = getTVars t
  getTVars (ps :=> t) = concatMap (\(IsIn _ ty) -> concatMap getTVars ty) ps ++ getTVars t
  getTVars _ = []

  containsTVar :: Int -> Type -> Bool
  containsTVar x (TVar x') = x == x'
  containsTVar i (TApp t1 t2) = containsTVar i t1 || any (containsTVar i) t2
  containsTVar i (t1 :-> t2) = all (containsTVar i) t1 || containsTVar i t2
  containsTVar i _ = False

  appearsInTC :: Type -> Type -> [Class]
  appearsInTC (TVar t) (ps :=> _) = filter (\(IsIn _ p) -> any (containsTVar t) p) ps
  appearsInTC t (t1 :-> t2) = concatMap (appearsInTC t) t1 ++ appearsInTC t t2
  appearsInTC _ _ = []

  createInstName :: [Class] -> String
  createInstName (IsIn name ty:ts) = name ++ createTypeInstName ty ++ createInstName ts
  createInstName [] = ""

  containsTVar' :: Type -> Bool
  containsTVar' (TVar _) = True
  containsTVar' (TApp t1 t2) = containsTVar' t1 || any containsTVar' t2
  containsTVar' (t1 :-> t2) = all containsTVar' t1 || containsTVar' t2
  containsTVar' (RefT t) = containsTVar' t
  containsTVar' (ListT t) = containsTVar' t
  containsTVar' _ = False

  createTypeInstName :: [Type] -> String
  createTypeInstName (t:ts) = case t of
    TVar x -> show x ++ (if null ts then "" else "_") ++ createTypeInstName ts
    TApp n ts -> createTypeInstName [n] ++ createTypeInstName ts
    TId n -> n
    _ -> show t ++ createTypeInstName ts
  createTypeInstName _ = ""

  appify :: Class -> Type
  appify (IsIn c ty) = TApp (TId c) ty

  -- Type checking a statement
  tyStatement :: MonadType m => Located Statement -> m (Maybe Type, Substitution, Env, [A.TypedStatement])
  tyStatement (Record name annot fields :> pos) = do
    generic <- mapM (const fresh) annot
    -- Recreating a map mapping generic name to type
    let table = M.fromList (zip annot generic)
    let header = if null table then TId name else TApp (TId name) $ M.elems table
    let fields' = map (\(n, e) -> (n, replace e table)) fields
    let ty = TRec fields'
    (map', env) <- ask
    let env' = B.second (M.insert name (generalize env ([ty] :-> header))) env
    return (Nothing, M.empty, env', [A.Record (name, header) (map (uncurry (A.:@)) fields')])
  tyStatement (Modified name value :> pos) = do
    (_, env) <- ask
    (t1, s1, env1, n1) <- tyExpression name
    traceShowM t1
    (t2, s2, env2, n2) <- tyExpression value
    case mgu t1 (RefT t2) of
      Right s -> do
        let s3 = s `compose` s2 `compose` s1
        return (Nothing, s3, applyEnv s3 $ env2 `union` env1 `union` env, apply s3 [A.Modified n1 n2])
      Left err -> throwError (err, Nothing, pos)
  tyStatement z@((Assignment (name :@ ty) value) :> pos) = do
    -- Fresh type for recursive definitions
    (map', e@(env, cons)) <- ask
    (tv, e) <- case ty of
      Just t -> do
        t' <- generalize e <$> createType t map'
        t' <- tyInstantiate t'
        return (t', M.singleton name (Forall [] t'))
      Nothing -> do
        tv <- fresh
        return (tv, M.singleton name (Forall [] tv))
    (t1', s1, e1, v1) <- local (B.second $ B.first (`M.union` e)) $ tyExpression value
    (v'', args, preds) <- resolveInstances (v1 :> pos)
    let t1 = case t1' of
          _ :=> ty -> if null preds then ty else L.nub preds :=> ty
          _ -> if null preds then t1' else L.nub preds :=> t1'

    s' <- case mgu (apply s1 t1) (apply s1 tv) of
      Left err -> throwError (err, Nothing, pos)
      Right s -> return s

    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
      Just t' -> do
        t'' <- tyInstantiate t'
        case mgu t1 t'' of
          Right s -> do
            let r = apply s t''
            return (s `compose` s' `compose` s1, r)
          Left x -> throwError (x, Just "Check for declarations and check types", pos)
      Nothing -> return (s1, t1)

    let s4 = s3 `compose` s' `compose` s1

    case apply s4 t2 of
      _ :-> _ -> return ()
      t -> when (containsTVar' t) $ 
        throwError ("Type " ++ show t ++ " contains type variables", Nothing, pos)

    let env'  = M.delete name env
        t'    = generalize (applyEnv s4 (env, cons)) (apply s4 t2)
        env'' = M.insert name t' env'

    let t2' = case t2 of
          _ :=> ty -> if null preds then ty else map appify (L.nub preds) :-> ty
          _ -> if null preds then t1' else map appify (L.nub preds) :-> t1'

    return (Nothing, s4, (env'', cons), [A.Assignment (name A.:@ apply s4 t2') (apply s4 $ if not (null args) then A.Lambda (L.nub $ map annotate args) (A.Return v'') t2' else v'')])
  tyStatement (Sequence stmts :> pos) = do
    (t1, s1, e1, v1) <- foldlM (\(t, s, e, v) stmt -> do
      (t', s', e', v') <- local (B.second (e `union`)) $ tyStatement stmt
      case check s s' of
        Right s'' -> return (t', s'', e' `union` e, v ++ [v'])
        Left err -> throwError (err, Nothing, pos)) (Nothing, M.empty, (M.empty, M.empty), []) stmts
    return (t1, s1, e1, [A.Sequence $ concat v1])
  tyStatement (If cond thenStmt elseStmt :> pos) = do
    (t1, s1, e1, v1) <- tyExpression cond
    (t2, s2, e2, v2) <- tyStatement thenStmt
    (t3, s3, e3, v3) <- tyStatement elseStmt
    let s4 = s1 `compose` s2 `compose` s3
    case mgu <$> t2 <*> t3 of
      Just (Right s) -> do
        let s5 = s `compose` s4
        case mgu (apply s5 t1) Bool of
          Right _ -> return (apply s5 t2, s5, apply s5 $ e1 `union` e2 `union` e3, [A.If v1 (A.Sequence v2) (A.Sequence v3)])
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
    return (Just t1, s1, e1, [A.Return v1])
  tyStatement (Expression expr :> pos) = do
    (t1, s1, e1, v1) <- tyExpression (expr :> pos)
    return (Nothing, s1, e1, [A.Expression v1])
  tyStatement (Enum name annot values :> pos) = do
    -- Creating a fresh type foreach type of reunion
    generic <- mapM (const fresh) annot
    -- Recreating a map mapping generic name to type
    let table = M.fromList (zip annot generic)
    let header = TApp (TId name) $ M.elems table
    let tvs = map (\(name, ty) -> case ty of
          Just t -> do
            let t' = map (`replace` table) t
              in (name A.:@ (t' :-> header))
          Nothing -> name A.:@ header) values
    (_, env) <- ask
    let cons = M.fromList $ map (\(x A.:@ ty) -> (x, generalize env ty)) tvs
    return (Nothing, M.empty, B.second (M.union cons) env, [A.Enum (name, TApp (TId name) (M.elems table)) tvs])
  tyStatement (Extern annot n ret :> pos) = do
    -- Creating a fresh type foreach type of reunion
    generic <- mapM (const fresh) annot
    -- Recreating a map mapping generic name to type
    let table = M.fromList (zip annot generic)
    ret' <- createType ret table
    (map', env) <- ask
    return (Nothing, M.empty, B.first (M.insert n (generalize env ret')) env, [A.Extern n ret'])
  tyStatement z@(Match expr cases :> pos) = do
    (pat_t, s1, e, pat') <- tyExpression expr

    (sub, res) <- foldM (\(s, acc) (pattern, expr) -> do
      (p, s', t, m) <- tyPattern pattern
      let s2 = s' `compose`  s
      -- (Type, Substitution, Env, A.TypedExpression)
      (t', s'', _, e) <- local (B.second . applyTypes $ apply s2 . (m `M.union`)) $ tyStatement expr
      let s3 = s'' `compose` s2
      case t' of
        Nothing -> throwError ("Pattern does not return anything", Nothing, pos)
        Just t' -> return (s3, acc ++ [(apply s3 t, apply s3 t', s3, (apply s3 p, apply s3 e))])) (s1, []) cases

    if null res
      then throwError ("No case matches in pattern matching", Nothing, pos)
      else do
        let (_, t, _, _) = head res

        let s = foldl (\acc (tp, te, s, _) ->
                let r = compose <$> mgu t te <*> mgu tp pat_t
                    r' = compose <$> r <*> acc
                  in compose <$> r' <*> pure s) (Right sub) res
        let s2 = foldl (\acc (tp, te, s, _) ->
                let r = compose <$> mgu t te <*> mgu tp pat_t
                    r' = compose <$> r <*> acc
                  in compose <$> r' <*> pure s) (Right sub) $ reverse res
        let s' = compose <$> s <*> s2

        -- Checking against patterns
        let tys = map (\(x, _, _, _) -> case s of
                    Right s -> apply s x
                    Left _ -> x) res

        let s'' = foldl (\acc x -> compose <$> patUnify x tys <*> acc) (Right M.empty) tys

        -- Checking against bodys
        let bodys = map (\(_, x, _, _) -> case s of
                    Right s -> apply s x
                    Left _ -> x) res

        let s''' = foldl (\acc x -> compose <$> patUnify x bodys <*> acc) (Right M.empty) bodys

        case compose <$> (compose <$> s'' <*> s''') <*> s' of
          Right s -> do
            let patterns' = map (\(_, _, _, (x, y)) -> (apply s x, apply s (A.Sequence y))) res

            -- (Maybe Type, Substitution, Env, A.TypedStatement)
            return (Just $ apply s t, s, (M.empty, M.empty), [A.Match pat' patterns'])
          Left e -> throwError (e, Nothing, pos)
  tyStatement (Class annots name fields :> pos) = do
    -- Creating a fresh type foreach type of reunion
    generic <- mapM (const fresh) annots
    -- Recreating a map mapping generic name to type
    let table = M.fromList (zip annots generic)
    let header = TApp (TId name) $ M.elems table
    let cls = IsIn name (M.elems table)

    let tvs = map (\(name, ty) -> let ty' = replace ty table in (name A.:@ ([cls] :=> ty'))) fields
    (_, env) <- ask
    let ty = map (\(name A.:@ ty) -> ty) tvs
    let cons = M.fromList $ zipWith (\x ty -> (x, generalize env ty)) (map fst fields) ty
    let datType = TApp (TId name) $ M.elems table
    let patterns = A.AppP name (map (uncurry A.VarP . unannotate) tvs)

    let env' = applyTypes (`M.union` M.fromList (map (B.second (generalize env) . unannotate) tvs)) env
    let enum = A.Enum (name, header) [name A.:@ (ty :-> datType)]
    let functions = map ((\(name, ty) ->
          A.Assignment
            (name A.:@ ([datType] :-> (case ty of
                  cls :=> ty -> ty
                  _ -> ty
                  )))
            (A.Lambda ["$s" A.:@ datType]
              (A.Match (A.Variable "$s" datType) [
                (patterns, A.Return (A.Variable name (case ty of
                  cls :=> ty -> ty
                  _ -> ty
                  )))
              ]) (case ty of
                  cls :=> ty -> ty
                  _ -> ty
                  ))) . unannotate) tvs
    return (Nothing, M.empty, env', enum : functions)
  tyStatement (Instance subs name ty fields :> pos) = do
    argsMap <- mapM (const fresh) subs
    let table = M.fromList (zip (map fst subs) argsMap)
    let subClasses = concatMap (\(name, ty) -> map (\x -> IsIn x [table M.! name]) ty) subs

    header <- createType ty table
    let cls' = IsIn name [header]
    let name' = createInstName [cls']

    fields' <- mapM (\(name', method) ->
      ask >>= \(_, (t, _)) -> case M.lookup name' t of
        Just ty' -> do
          -- create an instance of the method
          tv <- fresh
          ty <- tyInstantiate ty'

          (t1, s1, e, v') <- tyExpression method
          -- unifying it with instantatied method type
          let t' = (cls' : subClasses) :=> tv
          let s2 = compose <$> mgu t1 t' <*> mgu ty t1
          case s2 of
            Right s -> do
              (_, e) <- ask
              -- returning inferred type, inferred value, inferred method scheme and substitution
              return ((apply s t1, apply s v'), M.singleton name' (generalize e (apply s t1)), s)
            Left x -> throwError (x, Nothing, pos)
        Nothing -> throwError ("Unknown variable " ++ name' ++ " in " ++ name ++ " implementation.", Nothing, pos)) fields

    let tys = map (\((ty, _), _, _) -> ty) fields'
    let fields'' = map (\((_, f), _, _) -> f) fields'

    let s = map (\(_, _, s) -> s) fields'
    let s' = foldl1 compose s

    freshInstance (apply s' [cls'], (name', apply s' subClasses))

    (fields'', args, tys) <- unzip3 <$> forM (zip fields'' tys) (\(v', t1') -> do
      (v'', args, preds) <- resolveInstances (v' :> pos)
      let t1 = case t1' of
            _ :=> ty -> if null preds then ty else preds :=> ty
            _ -> if null preds then t1' else preds :=> t1'
      return (v'', args, t1))

    let datType = apply s' $ TApp (TId name) [apply s' header]
    let ty' = (cls' : subClasses) :=> (tys :-> datType)
    let call = A.FunctionCall (A.Constructor name ty') (apply s' fields'') datType

    let subConstraints = map (\(IsIn cls ty) -> TApp (TId cls) $ apply s' ty) subClasses
    let subNames = map (\(IsIn cls ty) -> cls ++ createTypeInstName (apply s' ty)) subClasses
    let args' = L.nub $ concat args
    return (Nothing, M.empty, (M.empty, M.empty), [A.Assignment (name' A.:@ datType) (if null args' then call else A.Lambda (map annotate args') (A.Return call) (map snd args' :-> datType))])

  resolveInstancesStmt :: MonadType m => Located A.TypedStatement -> m (A.TypedStatement, [(String, Type)], [Class])
  resolveInstancesStmt (A.Assignment (name A.:@ ty) expr :> pos) = do
    (e', tcs, cls) <- resolveInstances (expr :> pos)
    return (A.Assignment (name A.:@ ty) e', tcs, cls)
  resolveInstancesStmt (A.Return expr :> pos) = do
    (e', tcs, cls) <- resolveInstances (expr :> pos)
    return (A.Return e', tcs, cls)
  resolveInstancesStmt (A.Sequence exprs :> pos) = do
    (e', tcs, cls) <- unzip3 <$> mapM (resolveInstancesStmt . (:> pos)) exprs
    return (A.Sequence e', concat tcs, concat cls)
  resolveInstancesStmt (A.If cond then' else' :> pos) = do
    (cond', tcs, cls) <- resolveInstances (cond :> pos)
    (then'', tcs', cls') <- resolveInstancesStmt (then' :> pos)
    (else'', tcs'', cls'') <- resolveInstancesStmt (else' :> pos)
    return (A.If cond' then'' else'', tcs ++ tcs' ++ tcs'', cls ++ cls' ++ cls'')
  resolveInstancesStmt (A.Expression e :> pos) = do
    (e', tcs, cls) <- resolveInstances (e :> pos)
    return (A.Expression e', tcs, cls)
  resolveInstancesStmt (A.Match expr cases :> pos) = do
    (expr', tcs, cls) <- resolveInstances (expr :> pos)
    (cases', tcs', cls') <- unzip3 <$> mapM (\(p, e) -> do
      (e', tcs', cls') <- resolveInstancesStmt (e :> pos)
      return ((p, e'), tcs', cls')) cases
    return (A.Match expr' cases', tcs ++ concat tcs', cls ++ concat cls')
  resolveInstancesStmt (A.Modified n e :> pos) = do
    (e', tcs, cls) <- resolveInstances (e :> pos)
    return (A.Modified n e', tcs, cls)
  resolveInstancesStmt (x :> _) = return (x, [], [])

  resolveInstances :: MonadType m => Located A.TypedExpression -> m (A.TypedExpression, [(String, Type)], [Class])
  resolveInstances (A.Variable n t :> pos) = case t of
    cls :=> ty -> do
      env <- instances
      (calls, tcs, preds) <- find' pos cls env
      return (if not (null calls) then A.FunctionCall (A.Variable n (map appify (L.nub cls) :-> ty)) calls ty else A.Variable n t, L.nub tcs, L.nub preds)
    _ -> return (A.Variable n t, [], [])
  resolveInstances (A.FunctionCall e x t :> pos) = do
    (e', tcs, cls) <- resolveInstances (e :> pos)
    (x', tcs', cls') <- unzip3 <$> mapM (resolveInstances . (:> pos)) x
    return (A.FunctionCall e' x' t, L.nub $ tcs ++ concat tcs', L.nub $ cls ++ concat cls')
  resolveInstances (A.Lambda xs e t :> pos) = do
    (e', tcs, cls) <- resolveInstancesStmt (e :> pos)
    return (A.Lambda xs e' t, tcs, cls)
  resolveInstances (A.LetIn (x A.:@ t) e b ty :> pos) = do
    (e', tcs, cls) <- resolveInstances (e :> pos)
    let t1 = case t of
          _ :=> ty -> if null cls then ty else map appify (L.nub cls) :-> ty
          _ -> if null cls then t else map appify (L.nub cls) :-> t
    (b', tcs', cls') <- resolveInstances (b :> pos)
    return (A.LetIn (x A.:@ t1) (if null tcs then e' else A.Lambda (map annotate $ L.nub tcs) (A.Return e') t1) b' ty, L.nub tcs', L.nub cls')
  resolveInstances (x :> _) = return (x, [], [])

  annotate :: (String, Type) -> A.Annoted String
  annotate (x, t) = x A.:@ t

  applyForall :: Substitution -> Scheme -> Scheme
  applyForall s (Forall tvs t) = Forall tvs $ apply s t

  tyExpression :: (MonadType m, MonadFail m) => Located Expression -> m (Type, Substitution, Env, A.TypedExpression)
  tyExpression (LetIn (name :@ ty) value body :> pos) = do
    -- Fresh type for recursive definitions
    (map', e@(env, cons)) <- ask
    (tv, e) <- case ty of
      Just t -> do
        t' <- generalize e <$> createType t map'
        t' <- tyInstantiate t'
        return (t', M.singleton name (Forall [] t'))
      Nothing -> do
        tv <- fresh
        return (tv, M.singleton name (Forall [] tv))
    (t1, s1, e1, v1) <- local (B.second $ B.first (`M.union` e)) $ tyExpression value

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

    (t3, s2, e2, b') <- local (B.second . applyTypes . const $ apply s3 env'') $ tyExpression body
    let s4 = compose s2 s3
    return (t2, s4, apply s4 (env'', cons), A.LetIn (name A.:@ apply s4 t2) v1 b' t3)
  tyExpression (Variable name cast :> pos) = do
    (map', (env, cons)) <- ask
    case M.lookup name env of
      Just t@(Forall tvs _) -> do
        let cast' = map (`replace` map') cast
        let s = M.fromList $ zip tvs cast'
        t' <- tyInstantiate (applyForall s t)
        return (t', M.empty, (M.empty, M.empty), A.Variable name t')
      Nothing -> case M.lookup name cons of
        Just t -> do
          t' <- tyInstantiate t
          return (t', M.empty, (M.empty, M.empty), A.Constructor name t')
        Nothing -> throwError (
          "Variable " ++ name ++ " is not defined",
          Just "Check for declarations and check types", pos)
  tyExpression (Index e i :> pos) = do
    (t1, s1, e1, v1) <- tyExpression e
    (t2, s2, e2, v2) <- tyExpression i
    case mgu (apply s1 t2) Int of
      Right s -> do
        let s3 = s `compose` s1 `compose` s2
        tv <- fresh
        case mgu (apply s3 t1) (RefT tv) of
          Right s4 -> do
            return (apply s4 tv, s4 `compose` s3, (e1 `union` e2), A.Index v1 v2 (apply s4 tv))
          Left x -> throwError (x, Just "Make sure you passed a list or a string", pos)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Lambda annot args body :> pos) = do
    -- Creating a fresh type foreach type of reunion
    generic <- mapM (const fresh) annot
    -- Recreating a map mapping generic name to type
    let table = M.fromList (zip annot generic)

    -- Creating a new argument type list based on map
    tvs <- foldlM (\t (_ :@ ty) -> case ty of
      Just t' -> return $ t ++ [replace t' table]
      Nothing -> fresh >>= \t' -> return $ t ++ [t']) [] args

    (map', (env, cons)) <- ask
    let args' = map (\(x :@ _) -> x) args
    let env'  = foldl (flip M.delete) env args'
        env'' = env' `M.union` M.fromList (zipWith (\x t -> (x, Forall [] t)) args' tvs)
    (t1, s1, _, b) <- local (const (table `M.union` map', (env'', cons))) $ tyStatement body

    case t1 of
      Just t -> do
        let argTy = apply s1 tvs
        let argTy' = L.nub $ concatMap (\t -> 
              let res = concatMap (appearsInTCStmt (A.Sequence b)) (getTVars t)
                in if not (null res) then res else []) argTy
        return (apply s1 $ argTy' :=> (argTy :-> t), s1, (env, cons), A.Lambda (zipWith (A.:@) args' argTy) (apply s1 (A.Sequence b)) (apply s1 $ argTy' :=> (argTy :-> t)))
      Nothing -> throwError (
        "Lambda does not return anything",
        Just "Try to add expressions or statements in body", pos)
  tyExpression (z@(FunctionCall n xs) :> pos) = do
    tv <- fresh
    (t1, s1, e1, n1) <- tyExpression n
    (_, e) <- ask
    (t2, s2, e2, args) <- foldlM (\(t, s, e, a) x -> do
      (t', s', e', a') <- local (B.second $ apply s) $ tyExpression x
      return (t ++ [t'], s' `compose` s, e' `union` e, a ++ [a'])) ([], s1, e, []) xs

    let cls' = concatMap (\case
                cls :=> _ -> cls
                _ -> []) t2 ++ (case t1 of
                  cls :=> _ -> cls
                  _ -> [])
    case mgu (apply s2 t1) (t2 :-> tv) of
      Right s3 -> do
        return (apply s3 tv, s3 `compose` s2 `compose` s1, e, apply s3 $ A.FunctionCall n1 args (apply s3 (cls' :=> (t2 :-> tv))))
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Literal l :> _) = do
    (_, env) <- ask
    ty <- tyLiteral l
    return (ty, M.empty, env, A.Literal (compileLit l) ty)
  tyExpression ((BinaryOp op e1 e2) :> pos) = do
    (t1, s1, e1, v1) <- tyExpression (FunctionCall (Variable op [] :> pos) [e1, e2] :> pos)
    case v1 of
      A.FunctionCall (A.Variable op t) [e1', e2'] ty ->
        return (t1, s1, e1, A.BinaryOp op e1' e2' t1)
      _ -> throwError ("Error that should not happen", Nothing, pos)
  tyExpression (List elems :> pos) = do
    ls <- mapM tyExpression elems
    (_, env) <- ask
    case ls of
      [] -> do
        t <- fresh
        return (ListT t, M.empty, env, A.List (map (\(_, _, _, e) -> e) ls) (ListT t))
      (x:xs) ->
        let (x',_,_,_) = x
            xs' = map (\(x, _, _,_) -> x) xs
          in case doesUnify x' xs' of
            Left err -> throwError (err, Just "All elements of a list should have same type", pos)
            Right s -> return (ListT x', s, env, A.List (map (\(_, _, _, e) -> e) ls) (ListT x'))
    where
      doesUnify :: Type -> [Type] -> Either String Substitution
      doesUnify t [] = Right M.empty
      doesUnify t (x:xs) = case mgu t x of
        Right s -> compose s <$> doesUnify t xs
        Left err -> Left err
  tyExpression (Structure name fields :> pos) = do
    (_, (_, cons)) <- ask
    case M.lookup name cons of
      Just t -> do
        ([t'] :-> struct) <- tyInstantiate t
        (ts, s, e, f) <- unzip4 <$> mapM (\(n, e) -> do
          (t, s, env, f) <- tyExpression e
          return ((n, t), s, env, (n, f))) fields
        let s2 = foldl compose M.empty s
        case mgu (apply s2 t') (TRec ts) of
          Right s3 -> do
            return (apply s3 struct, s3, foldl union (M.empty, M.empty) e, A.Structure name f (apply s3 struct))
          Left err -> throwError (err, Just "Check the fields of the structure", pos)
      Nothing -> throwError (
        "Structure " ++ name ++ " is not defined",
        Just "Check for declarations and check types", pos)
  tyExpression (Object var property :> pos) = do
    env <- ask
    (t, s1, e, v1) <- tyExpression var
    tv <- fresh
    t' <- case t of
      TId n -> ask >>= \(_, (_, cons)) -> case M.lookup n cons of
        Just t -> tyInstantiate t >>= \case
          ([t''] :-> tv) -> return t'' 
          _ -> throwError ("Error that should not happen", Nothing, pos)
        Nothing -> throwError (n ++ " is not declared", Nothing, pos)
      _ -> return t
    let s2 = mgu t' (TRec [(property, tv)])
    let s3 = compose <$> s2 <*> pure s1
    case s3 of
      Right s ->
        return (apply s tv, s, apply s e, A.Object v1 property (apply s tv))
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Ternary cond e1 e2 :> pos) = do
    (t1, s1, env1, c) <- tyExpression cond
    (t2, s2, env2, t) <- tyExpression e1
    (t3, s3, env3, e) <- tyExpression e2
    let s4 = s1 `compose` s2 `compose` s3
    case mgu t3 t2 of
      Right s -> do
        let s5 = s `compose` s4
        case mgu (apply s5 t1) Bool of
          Right _ -> return (apply s5 t2, s5, apply s5 $ env1 `union` env2 `union` env3, apply s5 $ A.Ternary c t e (apply s5 t2))
          Left x -> throwError (
            x, Just "Expression should return a boolean type",
            getPosition cond)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Reference n :> pos) = do
    (t, s, e, n1) <- tyExpression n
    return (RefT t, s, e, A.Reference n1 (RefT t))
  tyExpression (Unreference n :> pos) = do
    ty <- fresh
    (t, s, e, n1) <- tyExpression n
    case mgu t (RefT ty) of
      Right s' -> do
        let s2 = s' `compose` s
        return (apply s2 ty, s2, apply s2 e, A.Unreference n1 (apply s2 ty))
      Left x -> throwError (x, Nothing, pos)
  tyExpression x = error $ "No supported yet: " ++ show x

  appearsInTC' :: A.TypedExpression -> Type -> [Class]
  appearsInTC' (A.Variable _ t) ty = appearsInTC ty t
  appearsInTC' (A.Literal _ t) ty = appearsInTC ty t
  appearsInTC' (A.BinaryOp _ e1 e2 t) ty = appearsInTC ty t ++ appearsInTC' e1 ty ++ appearsInTC' e2 ty
  appearsInTC' (A.List es t) ty = appearsInTC ty t ++ concatMap (`appearsInTC'` ty) es
  appearsInTC' (A.Structure _ es t) ty = appearsInTC t ty ++ concatMap ((`appearsInTC'` ty ). snd) es
  appearsInTC' (A.Object e _ t) ty = appearsInTC ty t ++ appearsInTC' e ty
  appearsInTC' (A.Ternary e1 e2 e3 t) ty = appearsInTC ty t ++ appearsInTC' e1 ty ++ appearsInTC' e2 ty ++ appearsInTC' e3 ty
  appearsInTC' (A.Reference e t) ty = appearsInTC ty t ++ appearsInTC' e ty
  appearsInTC' (A.Unreference e t) ty = appearsInTC ty t ++ appearsInTC' e ty
  appearsInTC' (A.FunctionCall e es t) ty = appearsInTC ty t ++ appearsInTC' e ty ++ concatMap (`appearsInTC'` ty) es
  appearsInTC' (A.Lambda args e t) ty = appearsInTC ty t ++ concatMap ((`appearsInTC` ty) . snd . unannotate) args ++ appearsInTCStmt e ty
  appearsInTC' (A.LetIn (n A.:@ t) e1 e2 t') ty = appearsInTC ty t ++ appearsInTC ty t' ++ appearsInTC' e1 ty ++ appearsInTC' e2 ty
  appearsInTC' (A.Constructor _ t) ty = appearsInTC ty t
  appearsInTC' (A.UnaryOp _ e t) ty = appearsInTC ty t ++ appearsInTC' e ty
  appearsInTC' (A.Index e1 e2 t) ty = appearsInTC ty t ++ appearsInTC' e1 ty ++ appearsInTC' e2 ty

  appearsInTCStmt :: A.TypedStatement -> Type -> [Class]
  appearsInTCStmt (A.Expression e) ty = appearsInTC' e ty
  appearsInTCStmt (A.If e1 e2 e3) ty = appearsInTC' e1 ty ++ appearsInTCStmt e2 ty ++ appearsInTCStmt e3 ty
  appearsInTCStmt (A.Sequence ss) ty = concatMap (`appearsInTCStmt` ty) ss
  appearsInTCStmt (A.Assignment (n A.:@ t) v) ty = appearsInTC ty t ++ appearsInTC' v ty
  appearsInTCStmt (A.Return e) ty = appearsInTC' e ty
  appearsInTCStmt (A.Modified n e) ty = appearsInTC' e ty ++ appearsInTC' n ty
  appearsInTCStmt (A.Match e cases) ty = appearsInTC' e ty ++ concatMap (\(_, s) -> appearsInTCStmt s ty) cases
  appearsInTCStmt (A.Enum _ _) _ = []
  appearsInTCStmt (A.Record _ _) _ = []
  appearsInTCStmt (A.Extern _ _) _ = []
  
  patUnify :: Type -> [Type] -> Either String Substitution
  patUnify x = foldl (\acc y -> compose <$> mgu x y <*> acc) (Right M.empty)

  tyPattern :: MonadType m => Located Expression -> m (A.TypedPattern, Substitution, Type, M.Map String Scheme)
  tyPattern (Variable "_" cast :> _) = do
    t <- fresh
    return (A.WilP, M.empty, t, M.empty)
  tyPattern (Variable "true" _ :> _) = return (A.VarP "true" Bool, M.empty, Bool, M.empty)
  tyPattern (Variable "false" _ :> _) = return (A.VarP "false" Bool, M.empty, Bool, M.empty)
  tyPattern (Variable n cast :> _) = ask >>= \(_, (_, c)) -> case M.lookup n c of
    Just t -> do
      t' <- tyInstantiate t
      return (A.VarP n t', M.empty, t', M.empty)
    Nothing -> do
      t <- fresh
      return (A.VarP n t, M.empty, t, M.singleton n (Forall [] t))
  tyPattern z@(FunctionCall e@(Variable n cast :> _) xs :> pos) = do
    tv <- fresh
    (n', s1, t1, m1) <- tyPattern e
    (x', s2, t2, m2) <- foldlM (\(x', s, t, m) x -> do
      (x'', s', t', m') <- local (B.second (apply s)) $ tyPattern x
      return (x' ++ [x''], s `compose` s', t ++ [t'], m' `M.union` m)) ([], s1, [], M.empty) xs
    case mgu (t2 :-> tv) (apply s2 t1)  of
      Right s3 -> do
        let x'' = apply s3 tv
        return (A.AppP n x', s3 `compose` s2 `compose` s1, x'', m1 `M.union` m2)
      Left x -> throwError (x, Nothing, pos)

  tyPattern (Literal (S (s :> _)) :> _) = return (A.LitP (A.S s), M.empty, ListT Char, M.empty)
  tyPattern (Literal (I (i :> _)) :> _) = return (A.LitP (A.I i), M.empty, Int, M.empty)
  tyPattern (Literal (F (f :> _)) :> _) = return (A.LitP (A.F f), M.empty, Float, M.empty)
  tyPattern (Literal (C (c :> _)) :> _) = return (A.LitP (A.C c), M.empty, Char, M.empty)
  tyPattern x = error $ "tyPattern: not implemented => " ++ show x

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

  runCheck :: (MonadIO m, MonadFail m) => [Located Statement] -> m (Either (String, Maybe String, (SourcePos, SourcePos)) [A.TypedStatement])
  runCheck stmt =
    ((\(_, x, _) -> x) <$>) <$> foldlM (\e x -> case e of
      Right (e, a, i) -> do
        x <- runExceptT $ runRWST (tyStatement x) (M.empty, e) i
        case x of
          Right ((_, _, e', a'), i, _) -> return (Right (e' `union` e, a ++ a', i))
          Left err -> return (Left err)
      Left err -> return (Left err)) (Right ((env, M.empty), [], (0, []))) stmt