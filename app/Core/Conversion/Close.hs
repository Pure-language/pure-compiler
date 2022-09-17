{-# LANGUAGE TupleSections #-}

module Core.Conversion.Close where
  import Core.TypeChecking.Type.AST
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.Pretty ()
  import Control.Monad.RWS
  import Data.Bifunctor (Bifunctor(second, first))
  import Core.Conversion.Free (Free(free))
  import qualified Data.Set as S
  import Debug.Trace (traceShowM)

  type MonadClosure m = (MonadRWS () [TypedStatement] (Int, (Int, ([(String, Type)], [String]))) m)

  runClosureConversion :: Monad m => [TypedStatement] -> m ([TypedStatement], [TypedStatement])
  runClosureConversion x = do
    (x, enums) <- evalRWST (mapM closeStmt x) () (0, (0, ([], [])))
    return (enums, x)

  addFunction :: MonadClosure m => String -> Type -> m ()
  addFunction name typ = do
    (i, (j, (xs, s))) <- get
    put (i, (j, ((name, typ) : xs, s)))

  freshName :: MonadClosure m => m String
  freshName = do
    n <- gets (fst . snd)
    modify (second $ first (+1))
    return ("$r" ++ show n)

  freshLambda :: MonadClosure m => m String
  freshLambda = do
    n <- gets fst
    modify (first (+1))
    return ("lambda" ++ show n)

  addExtern :: MonadClosure m => String -> m ()
  addExtern name = do
    (i, (j, (xs, s))) <- get
    put (i, (j, (xs, name : s)))

  closeStmt :: MonadClosure m => TypedStatement -> m TypedStatement
  closeStmt z@(Assignment (name :@ _) (Lambda args body t)) =
    if name /= "main"
      then do
        let env = free z
        body <- closeStmt body

        let env' = zipWith (\a c -> ("_" ++ show c) :@ snd a) (S.toList env) [0..]
        let name' = name ++ "Closure"
        let envType = TId name'

        let newLambdaType = case t of
              t2 :-> t1 -> (RefT envType : t2) :-> t1
              _ -> error "Invalid function type"
        let enum = Record (name', envType) (("fun" :@ newLambdaType) : env')

        tell [enum]

        let createVar :: (String, Type) -> Integer -> TypedStatement
            createVar (n, t) c = Assignment (n :@ t) (Object (Unreference (Variable "env" envType)) ("_" ++ show c))

        addFunction name (TId name')
        --traceShowM (l, body)


        let body' = case body of
                  Sequence stmts -> Sequence (zipWith createVar (S.toList env) [0..] ++ stmts)
                  _ -> Sequence (zipWith createVar (S.toList env) [0..] ++ [body])

        let envStruct = Structure (("fun", Lambda ("env" :@ RefT envType : args) body' newLambdaType) : zipWith (\(n, t) x -> ("_" ++ show x, Variable n t)) (S.toList env) [0..]) envType
        return $ Assignment (name :@ envType) envStruct
      else do
        body <- closeStmt body
        return $ Assignment (name :@ t) (Lambda args body t)

  closeStmt (Assignment n e) = do
    e <- fst <$> closeExpr e
    return $ Assignment n e
  closeStmt (Modified n e) = do
    e <- fst <$> closeExpr e
    return $ Modified n e
  closeStmt (If e s1 s2) = do
    e <- fst <$> closeExpr e
    s1 <- closeStmt s1
    s2 <- closeStmt s2
    return $ If e s1 s2
  closeStmt (Sequence s1) = do
    s1 <- mapM closeStmt s1
    return $ Sequence s1
  closeStmt (Expression e) = do
    e <- fst <$> closeExpr e
    return $ Expression e
  closeStmt (Return e) = do
    e <- fst <$> closeExpr e
    return $ Return e
  closeStmt (Extern name args t) = do
    addExtern name
    return $ Extern name args t
  closeStmt (Match e cases) = do
    (e, env) <- closeExpr e
    cases <- mapM (\(x, y) -> (x,) <$> closeStmt y) cases
    return $ Match e cases
  closeStmt x = return x

  functionCall n args ty = do
    -- Transforming call (M(N))(O) into 
    -- let $r1 = 
    --   let $r2 = M
    --     in $r2.fun($r2, N)
    --   in $r1.fun($r1, O)
    (n', env) <- closeExpr n
    (args', _) <- unzip <$> mapM closeExpr args

    r <- freshName

    let sType = case env of
          Just t -> TId t
          Nothing -> TRec [("fun", ty)]
    let ty' = case ty of
          t2 :-> t1 -> (sType : t2) :-> t1
          _ -> error "Invalid function type"

    let ret = case ty' of
          t2 :-> t1 -> t1
          _ -> error "Invalid function type"

    return (LetIn (r :@ sType) n'
      (FunctionCall (Object (Variable r sType) "fun") (Reference (Variable r sType) : args') ty') ret, Nothing)

  closeExpr :: MonadClosure m => TypedExpression -> m (TypedExpression, Maybe String)
  closeExpr (FunctionCall n args ty) = do
    case n of
      Variable name t -> gets (snd . snd . snd) >>= \env ->
        if name `notElem` env
          then functionCall n args ty
          else do
            (args', _) <- unzip <$> mapM closeExpr args
            return (FunctionCall n args' ty, Nothing)
      Constructor _ _ -> do
          (args', _) <- unzip <$> mapM closeExpr args
          return (FunctionCall n args' ty, Nothing)
      _ -> functionCall n args ty
  closeExpr z@(Lambda args body t) = do
    name' <- (++"Closure") <$> freshLambda
    let env = free z
    body <- closeStmt body

    let env' = zipWith (\a c -> ("_" ++ show c) :@ snd a) (S.toList env) [0..]
    let envType = TId name'

    let newLambdaType = case t of
          t2 :-> t1 -> (RefT envType : t2) :-> t1
          _ -> error "Invalid function type"
    let enum = Record (name', envType) (("fun" :@ newLambdaType) : env')

    tell [enum]

    let createVar :: (String, Type) -> Integer -> TypedStatement
        createVar (n, t) c = Assignment (n :@ t) (Object (Unreference (Variable "env" envType)) ("_" ++ show c))

    let body' = case body of
              Sequence stmts -> Sequence (zipWith createVar (S.toList env) [0..] ++ stmts)
              _ -> Sequence (zipWith createVar (S.toList env) [0..] ++ [body])

    --traceShowM (l, body)
    let envStruct = Structure (("fun", Lambda ("env" :@ RefT envType : args) body' newLambdaType) : zipWith (\(n, t) x -> ("_" ++ show x, Variable n t)) (S.toList env) [0..]) envType

    return (envStruct, Just name')
  closeExpr (BinaryOp op e1 e2) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    return (BinaryOp op e1 e2, Nothing)
  closeExpr (UnaryOp op e) = do
    (e, env) <- closeExpr e
    return (UnaryOp op e, env)
  closeExpr (List e) = do
    (e, env) <- unzip <$> mapM closeExpr e
    return (List e, Nothing)
  closeExpr (Index e1 e2) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    return (Index e1 e2, Nothing)
  closeExpr (LetIn n e1 e2 t) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    return (LetIn n e1 e2 t, Nothing)
  closeExpr (Structure s t) = do
    (s, env) <- unzip <$> mapM (\(x, i) -> do
      (i, env) <- closeExpr i
      return ((x, i), env)) s
    return (Structure s t, Nothing)
  closeExpr (Object e n) = do
    (e, env) <- closeExpr e
    return (Object e n, env)
  closeExpr (Ternary e1 e2 e3) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    (e3, env3) <- closeExpr e3
    return (Ternary e1 e2 e3, Nothing)
  closeExpr (Reference e) = do
    (e, env) <- closeExpr e
    return (Reference e, env)
  closeExpr (Unreference e) = do
    (e, env) <- closeExpr e
    return (Unreference e, env)
  closeExpr (Variable x t) = gets (fst . snd . snd) >>= \env -> case lookup x env of
    Just t@(TId n) -> return (Variable x t, Just n)
    Just _ -> error "Unexpected type"
    Nothing -> case t of
      t1 :-> t2 -> return (Variable x (TRec [("fun", t)]), Nothing)
      _ -> return (Variable x t, Nothing)
  closeExpr x = return (x, Nothing)