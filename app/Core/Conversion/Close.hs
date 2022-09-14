{-# LANGUAGE TupleSections #-}
module Core.Conversion.Close where
  import Core.TypeChecking.Type.AST
  import Core.TypeChecking.Type.Definition
  import Control.Monad.RWS
  import Data.Bifunctor (Bifunctor(second, first))
  import Core.Conversion.Free (Free(free))
  import qualified Data.Set as S
  import Debug.Trace (traceShowM)
  
  type MonadClosure m = (MonadRWS () () (Int, Int) m)

  runClosureConversion :: Monad m => [TypedStatement] -> m [TypedStatement]
  runClosureConversion x =
    fst <$> evalRWST (mapM closeStmt x) () (0, 0)

  freshName :: MonadClosure m => m String
  freshName = do
    n <- gets snd
    modify (second (+1))
    return ("$r" ++ show n)

  freshEnv :: MonadClosure m => m String
  freshEnv = do
    n <- gets fst
    modify (first (+1))
    return ("_" ++ show n)

  closeStmt :: MonadClosure m => TypedStatement -> m TypedStatement
  closeStmt z@(Assignment (name :@ _) (Lambda args body t)) = do
    let env = free z
    body <- closeStmt body
    
    let env' = zipWith (\a c -> ("_" ++ show c, snd a)) (S.toList env) [0..]
    let envType = TRec $ ("fun", t) : env'
    
    let createVar :: (String, Type) -> Integer -> TypedStatement
        createVar (n, t) c = Assignment (n :@ t) (Object (Variable "env" envType) ("_" ++ show c))
    
    let newLambdaType = case t of
          t2 :-> t1 -> (envType : t2) :-> t1
          _ -> error "Invalid function type"
    let body' = case body of
              Sequence stmts -> Sequence (zipWith createVar (S.toList env) [0..] ++ stmts)
              _ -> Sequence (zipWith createVar (S.toList env) [0..] ++ [body])

    --traceShowM (l, body)
    let envStruct = Structure (("fun", Lambda ("env" :@ envType : args) body' newLambdaType) : zipWith (\(n, t) x -> ("_" ++ show x, Variable n t)) (S.toList env) [0..]) envType

    return $ Assignment (name :@ envType) envStruct

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
  closeStmt x = return x

  closeExpr :: MonadClosure m => TypedExpression -> m (TypedExpression, S.Set (String, Type))
  closeExpr (FunctionCall n args ty) = do
    -- Transforming call (M(N))(O) into 
    -- let $r1 = 
    --   let $r2 = M
    --     in $r2.fun($r2, N)
    --   in $r1.fun($r1, O)
    (n', env) <- closeExpr n
    (args', _) <- unzip <$> mapM closeExpr args

    r <- freshName

    let sType = TRec $ ("fun", ty) : S.toList env
    let ty' = case ty of
          t2 :-> t1 -> (sType : t2) :-> t1
          _ -> error "Invalid function type"
    
    let ret = case ty' of
          t2 :-> t1 -> t1
          _ -> error "Invalid function type"

    return $ (LetIn (r :@ sType) n' 
      (FunctionCall (Object (Variable r sType) "fun") (Variable r sType : args') ty') ret, S.empty)
  closeExpr z@(Lambda args body t) = do
    let env = free z
    body <- closeStmt body
    
    let env' = zipWith (\a c -> ("_" ++ show c, snd a)) (S.toList env) [0..]
    let envType = TRec $ ("fun", t) : env'
    
    let createVar :: (String, Type) -> Integer -> TypedStatement
        createVar (n, t) c = Assignment (n :@ t) (Object (Variable "env" envType) ("_" ++ show c))
    
    let newLambdaType = case t of
          t2 :-> t1 -> (envType : t2) :-> t1
          _ -> error "Invalid function type"
    let body' = case body of
              Sequence stmts -> Sequence (zipWith createVar (S.toList env) [0..] ++ stmts)
              _ -> Sequence (zipWith createVar (S.toList env) [0..] ++ [body])

    --traceShowM (l, body)
    let envStruct = Structure (("fun", Lambda ("env" :@ envType : args) body' newLambdaType) : zipWith (\(n, t) x -> ("_" ++ show x, Variable n t)) (S.toList env) [0..]) envType

    return (envStruct, S.empty)
  closeExpr (BinaryOp op e1 e2) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    return (BinaryOp op e1 e2, env1 `S.union` env2)
  closeExpr (UnaryOp op e) = do
    (e, env) <- closeExpr e
    return (UnaryOp op e, env)
  closeExpr (List e) = do
    (e, env) <- unzip <$> mapM closeExpr e
    return (List e, S.unions env)
  closeExpr (Index e1 e2) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    return (Index e1 e2, env1 `S.union` env2)
  closeExpr (LetIn n e1 e2 t) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    return (LetIn n e1 e2 t, env1 `S.union` env2)
  closeExpr (Structure s t) = do
    (s, env) <- unzip <$> mapM (\(x, i) -> do
      (i, env) <- closeExpr i
      return ((x, i), env)) s
    return (Structure s t, S.unions env)
  closeExpr (Object e n) = do
    (e, env) <- closeExpr e
    return (Object e n, env)
  closeExpr (Ternary e1 e2 e3) = do
    (e1, env1) <- closeExpr e1
    (e2, env2) <- closeExpr e2
    (e3, env3) <- closeExpr e3
    return (Ternary e1 e2 e3, env1 `S.union` env2 `S.union` env3)
  closeExpr (Reference e) = do
    (e, env) <- closeExpr e
    return (Reference e, env)
  closeExpr (Unreference e) = do
    (e, env) <- closeExpr e
    return (Unreference e, env)
  closeExpr (Match e cases) = do
    (e, env) <- closeExpr e
    cases <- mapM (\(x, y) -> (x,) <$> closeStmt y) cases
    return (Match e cases, S.empty)
  closeExpr (Variable x t) = case t of
    t1 :-> t2 -> return (Variable x (TRec [("fun", t)]), S.empty)
    _ -> return (Variable x t, S.empty)
  closeExpr x = return (x, S.empty)