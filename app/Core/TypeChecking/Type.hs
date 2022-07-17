{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Core.TypeChecking.Type where
  import Control.Monad.RWS
  import Control.Monad.Except
  import Core.Parser.AST
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.Methods (compose)
  import qualified Data.Set as S
  import qualified Data.Map as M
  import Core.TypeChecking.Substitution (Types(free, apply), Substitution)
  import Data.Foldable (foldlM)
  import Core.TypeChecking.Unification (mgu, check)
  import Text.Parsec (SourcePos)
  import Data.Either (isLeft)
  import Data.Maybe (isNothing)
  import Control.Arrow (Arrow(second))
  
  getPosition :: Located a -> (SourcePos, SourcePos)
  getPosition (_ :> pos) = pos

  type MonadType m = (MonadRWS TypeEnv () Int m, MonadIO m, MonadError (String, Maybe String, (SourcePos, SourcePos)) m)

  generalize :: TypeEnv -> Type -> Scheme
  generalize env t = Forall (S.toList vars) t
    where vars = free t S.\\ free env

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
          (i', args') = foldl (\(i, m) a -> let (i', t) = getGenerics a m i in (i', t `M.union` m)) (i, m) args
  getGenerics (Array t) m i = getGenerics t m i
  getGenerics _ m i = (i, m)

  convert :: Declaration -> M.Map String Int -> Type
  convert (Generic a) m = case M.lookup a m of
    Just t -> TVar t
    Nothing -> error "Type variable not found"
  convert (Arrow args t) m = map (`convert` m) args :-> convert t m
  convert (Array t) m = ListT (convert t m)
  convert (StructE f) m = TRec (map (second (`convert` m)) f)
  convert IntE _ = Int
  convert CharE _ = Char
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

  buildType :: Declaration -> Scheme
  buildType d = do
    let (i, m) = getGenerics d M.empty 0
        ty     = convert d m
      in Forall (M.elems m) ty

  {- TYPE CHECKING -}

  -- Type checking a statement
  tyStatement :: MonadType m => Located Statement -> m (Maybe Type, Substitution, TypeEnv)
  tyStatement z@((Assignment (name :@ ty) value) :> pos) = do
    -- Fresh type for recursive definitions
    env <- ask
    (tv, e) <- case ty of
      Just t -> do
        t' <- tyInstantiate $ buildType t
        return (t', M.singleton name (Forall [] t'))
      Nothing -> do
        tv <- fresh
        return (tv, M.singleton name (Forall [] tv))
    (t1, s1, e1) <- local (`M.union` e) $ tyExpression value
    
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
        t'    = generalize (apply s3 env) (apply s3 t2)
        env'' = M.insert name t' env'
    return (Nothing, s3, env'')
  tyStatement (Sequence stmts :> pos) = do
    (t1, s1, e1) <- foldlM (\(t, s, e) stmt -> do
      (t', s', e') <- local (`M.union` e) $ tyStatement stmt
      case check s s' of
        Right s'' -> return (t', s'', e' `M.union` e)
        Left err -> throwError (err, Nothing, pos)) (Nothing, M.empty, M.empty) stmts
    return (t1, s1, e1)
  tyStatement (If cond thenStmt elseStmt :> pos) = do
    (t1, s1, e1) <- tyExpression cond
    (t2, s2, e2) <- tyStatement thenStmt
    (t3, s3, e3) <- tyStatement elseStmt
    let s4 = s1 `compose` s2 `compose` s3
    case mgu <$> t2 <*> t3 of
      Just (Right s) -> do
        let s5 = s `compose` s4
        case mgu (apply s5 t1) Bool of
          Right _ -> return (apply s5 t2, s5, apply s5 $ e1 `M.union` e2 `M.union` e3)
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
    (t1, s1, e1) <- tyExpression expr
    return (Just t1, s1, e1)

  tyStatement (Expression expr :> pos) = do
    (t1, s1, e1) <- tyExpression (expr :> pos)
    return (Just t1, s1, e1)

  tyExpression :: MonadType m => Located Expression -> m (Type, Substitution, TypeEnv)
  tyExpression (Variable name :> pos) = do
    env <- ask
    case M.lookup name env of
      Just t -> do
        t' <- tyInstantiate t
        return (t', M.empty, M.empty)
      Nothing -> throwError (
        "Variable " ++ name ++ " is not defined",
        Just $ "Try to define a new variable " ++ name, pos)
  tyExpression (Index e i :> pos) = do
    (t1, s1, e1) <- tyExpression e
    (t2, s2, e2) <- tyExpression i
    case mgu (apply s1 t2) Int of
      Right s -> do
        let s3 = s `compose` s1 `compose` s2
        tv <- fresh
        case mgu (apply s3 t1) (ListT tv) of
          Right s4 -> do
            return (apply s4 tv, s4 `compose` s3, e1 `M.union` e2)
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
      
    env <- ask
    let args' = map (\(x :@ _) -> x) args
    let env'  = foldl (flip M.delete) env args' 
        env'' = env' `M.union` M.fromList (zipWith (\x t -> (x, Forall [] t)) args' tvs)
    (t1, s1, _) <- local (const env'') $ tyStatement body
    case t1 of
      Just t -> do
        let argTy = apply s1 tvs
        return (argTy :-> t, s1, env)
      Nothing -> throwError (
        "Lambda does not return anything",
        Just "Try to add expressions or statements in body", pos)
  tyExpression (z@(FunctionCall n xs) :> pos) = do
    tv <- fresh
    (t1, s1, e1) <- tyExpression n
    e <- ask
    (t2, s2, e2) <- foldlM (\(t, s, e) x -> do
      (t', s', e') <- local (apply s) $ tyExpression x
      return (t ++ [t'], s `compose` s', M.union e' e)) ([], s1, e) xs
    case mgu (t2 :-> tv) (apply s2 t1) of
      Right s3 -> return (apply s3 tv, s3 `compose` s2 `compose` s1, apply s3 e)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Literal l :> _) = (,M.empty, M.empty) <$> tyLiteral l
  tyExpression ((BinaryOp op e1 e2) :> pos) =
    tyExpression (FunctionCall (Variable op :> pos) [e1, e2] :> pos)
  tyExpression (List elems :> pos) = do
    ls <- mapM tyExpression elems
    case ls of
      [] -> do
        t <- fresh
        return (ListT t, M.empty, M.empty)
      (x:xs) ->
        let (x',_,_) = x
            xs' = map (\(x, _, _) -> x) xs
          in case doesUnify x' xs' of
            Left err -> throwError (err, Just "All elements of a list should have same type", pos)
            Right s -> return (ListT x', s, M.empty)
    where
      doesUnify :: Type -> [Type] -> Either String Substitution
      doesUnify t [] = Right M.empty
      doesUnify t (x:xs) = case mgu t x of
        Right s -> compose s <$> doesUnify t xs
        Left err -> Left err
  tyExpression (Structure fields :> pos) = do
    (ts, s, e) <- unzip3 <$> mapM (\(n, e) -> do
      (t, s, env) <- tyExpression e
      return ((n, t), s, env)) fields
    return (TRec ts, foldl compose M.empty s, M.unions e)
  tyExpression (Object var property :> pos) = do
    env <- ask
    (t, s1, e) <- tyExpression var
    tv <- fresh
    let s2 = mgu t (TRec [(property, tv)])
    let s3 = compose <$> s2 <*> pure s1
    case s3 of
      Right s -> do
        return (apply s tv, s, apply s e)
      Left x -> throwError (x, Nothing, pos)
  tyExpression (Ternary cond e1 e2 :> pos) = do
    (t1, s1, env1) <- tyExpression cond
    (t2, s2, env2) <- tyExpression e1
    (t3, s3, env3) <- tyExpression e2
    let s4 = s1 `compose` s2 `compose` s3
    case mgu t2 t3 of
      Right s -> do
        let s5 = s `compose` s4
        case mgu (apply s5 t1) Bool of
          Right _ -> return (apply s5 t2, s5, apply s5 $ env1 `M.union` env2 `M.union` env3)
          Left x -> throwError (
            x, Just "Expression should return a boolean type",
            getPosition cond)
      Left x -> throwError (x, Nothing, pos)
  tyExpression x = error $ "No supported yet: " ++ show x

  tyLiteral :: MonadType m => Literal -> m Type
  tyLiteral (I _) = return Int
  tyLiteral (S _) = return $ ListT Char
  tyLiteral (F _) = return Float
  tyLiteral (C _) = return Char

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

  runCheck :: MonadIO m => [Located Statement] -> m (Either (String, Maybe String, (SourcePos, SourcePos)) ())
  runCheck stmt = do
    (\case
      Right _ -> Right ()
      Left err -> Left err) <$> foldlM (\e x -> case e of
      Right e -> do
        x <- runExceptT $ runRWST (tyStatement x) e 0
        case x of
          Right ((_, _, e), _, _) -> return (Right e)
          Left err -> return (Left err)
      Left err -> return (Left err)) (Right env) stmt