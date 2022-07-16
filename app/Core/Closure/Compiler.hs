module Core.Closure.Compiler where
  import Core.Closure.Conversion
  import qualified Core.Parser.AST as A
  import Control.Monad.RWS (gets, modify)
  import qualified Data.Map as M
  import Control.Monad.State (StateT(runStateT))
  import Debug.Trace (traceShow)
  import qualified Data.Foldable as S
  import Data.List (intersect)
  
  convertStmt :: MonadClosure m => A.Located A.Statement -> m Statement
  convertStmt (A.Assignment (name A.:@ _) (A.Lambda args body A.:> _) A.:> _) = do
    let args' = map (\(x A.:@ _) -> x) args
    name' <- fresh
    env <- gets environment
    addEnv name
    unionEnv args'
    body' <- convertStmt body
    setEnv env
    addClosure $ Assignment name' (Lambda (args' ++ (S.toList (free body) `intersect` env)) body')
    modify $ \s -> s { matches = M.insert name name' (matches s) }
    return $ Assignment name (Variable name')
  convertStmt (A.Assignment (name A.:@ _) value A.:> _) = do
    addEnv name
    body' <- convertExpr value
    return $ Assignment name body' 
  convertStmt (A.If cond then_ else_ A.:> _) =
    If <$> convertExpr cond <*> convertStmt then_ <*> convertStmt else_
  convertStmt (A.Sequence stmts A.:> _) = do
    env <- gets environment
    matches <- gets matches
    stmts' <- mapM convertStmt stmts
    setEnv env
    modify $ \s -> s { matches = matches }
    return $ Sequence stmts'
  convertStmt (A.Expression e A.:> pos) = 
    Expression <$> convertExpr (e A.:> pos)
  convertStmt (A.Return e A.:> _) = do
    Return <$> convertExpr e

  convertExpr :: MonadClosure m => A.Located A.Expression -> m Expression
  convertExpr (A.FunctionCall n xs A.:> _)
    = FunctionCall <$> convertExpr n <*> mapM convertExpr xs
  convertExpr (A.Lambda args body A.:> _) = do
    let args' = map (\(x A.:@ _) -> x) args
    name' <- fresh
    env <- gets environment
    unionEnv args'
    body' <- convertStmt body
    setEnv env
    addClosure $ Assignment name' (Lambda (args' ++ (S.toList (free body) `intersect` env)) body')
    return $ Variable name'
  convertExpr (A.Variable n A.:> _) = gets matches >>= \e -> case M.lookup n e of
    Just n' -> return $ Variable n'
    Nothing -> return $ Variable n
  convertExpr (A.Literal l A.:> _) = return . Literal $ convertLit l
  convertExpr (A.BinaryOp op e1 e2 A.:> _) =
    BinaryOp op <$> convertExpr e1 <*> convertExpr e2
  convertExpr (A.UnaryOp op e A.:> _) =
    UnaryOp op <$> convertExpr e
  convertExpr (A.List xs A.:> _) =
    List <$> mapM convertExpr xs
  convertExpr (A.Index e i A.:> _) =
    Index <$> convertExpr e <*> convertExpr i

  convertLit :: A.Literal -> Literal
  convertLit (A.I (i A.:> _)) = I i
  convertLit (A.F (f A.:> _)) = F f
  convertLit (A.S (s A.:> _)) = S s
  convertLit (A.C (s A.:> _)) = C s

  convertClosures :: Monad m => [A.Located A.Statement] -> m ([Statement], Closure)
  convertClosures z = runStateT (mapM convertStmt z) (Closure [] [] 0 M.empty)