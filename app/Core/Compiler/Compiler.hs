{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Compiler where
  import qualified Data.Map as M
  import Data.List (intercalate, union, (\\))
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.AST
  import Core.Compiler.CodeGen (IR(..), varify, isStatement)
  import Control.Monad.RWS (gets, modify)
  import Control.Monad.State (StateT(runStateT), evalStateT)
  import Core.Compiler.Modules.ADT (compileData)
  import Core.Compiler.Modules.Pattern (compileCase)
  import Core.TypeChecking.Type.Pretty ()
  import Control.Arrow (Arrow(second))
  import Debug.Trace (traceShowM, traceShow)
  import Core.Compiler.Type (MonadCompiler, getConstructor)
  import Core.TypeChecking.Unification (TypeState (modules), Module (Module))
  
  isValidOperator :: String -> Bool
  isValidOperator = (`elem` ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">="])

  analyseIRModule :: [IR] -> [String]
  analyseIRModule ((IRExport (IRDeclaration n _)):xs) = n : analyseIRModule xs
  analyseIRModule (_:xs) = analyseIRModule xs
  analyseIRModule [] = []

  compileToplevel :: MonadCompiler m => TypeState -> TypedStatement -> m [IR]
  compileToplevel s (Assignment (name :@ ty) e) = do
    e' <- compileExpression e
    return [IRDeclaration (varify name) e']
  compileToplevel s z@(Enum (n, ty) fields) = (:[]) <$> compileData z
  compileToplevel s (Extern n ret) = return []
  compileToplevel s (Record (name, ty) fields) = return []
  compileToplevel s (Import expr path) = do
    xs <- case M.lookup path $ modules s of 
      Just (Module _ stmts) -> 
        analyseIRModule . concat <$> mapM (compileToplevel s) stmts
      _ -> error . show $ M.keys $  modules s
    (:[]) . IRImport xs . IRAwait . IRCall (IRVariable "import") . (:[]) <$> compileExpression expr
  compileToplevel s (Public stmts) = do
    stmt <- compileToplevel s stmts
    return $ map IRExport stmt
  compileToplevel _ x = error $ "Not implemented: " ++ show x

  createIfSequence :: [IR]-> IR
  createIfSequence [] = error "Empty if sequence"
  createIfSequence [x] = x
  createIfSequence (IRIf cond then':xs) = IRIfElse cond then' (createIfSequence xs)
  createIfSequence _ = error "Invalid if sequence"

  compileStatement :: MonadCompiler m => TypedStatement -> m [IR]
  compileStatement (Assignment (name :@ ty) e) = do
    e' <- compileExpression e
    return [IRDeclaration (varify name) e']
  compileStatement (Modified n e) = do
    n' <- compileExpression n
    e' <- compileExpression e
    return [IRModification (IRStructProp n' "value") e']
  compileStatement (If e s1 s2) = do
    e' <- compileExpression e
    s1' <- compileExpression s1
    s2' <- compileExpression s2
    return [IRIfElse e' s1' s2']
  compileStatement (Expression e)
    = (:[]) <$> compileExpression e
  compileStatement (Return e)
    = (:[]) . IRReturn <$> compileExpression e
  compileStatement (For (n, _) e body) = do
    e' <- compileExpression e
    body' <- compileExpression body
    return [IRFor (varify n) e' body']
  compileStatement (While e body) = do
    e' <- compileExpression e
    body' <- compileExpression body
    return [IRWhile e' body']
  compileStatement Break = return [IRBreak]
  compileStatement Continue = return [IRContinue]
  compileStatement x = error $ "Not implemented: " ++ show x

  embed :: IR -> IR
  embed x = IRCall (IRLambda [] x) []

  compileExpression :: MonadCompiler m => TypedExpression -> m IR
  compileExpression (Variable "void" _) = return (IRVariable "null")
  compileExpression (Variable name t) = return $ IRVariable (varify name)
  compileExpression (FunctionCall call args ty) = do
    call' <- compileExpression call
    args' <- mapM compileExpression args
    return $ IRCall call' args'
  compileExpression (Reference c t) = IRLamStruct . (:[]) . ("value",) <$> compileExpression c
  compileExpression (Unreference c t) = IRDeref <$> compileExpression c
  compileExpression (Match x pats) = do
    x <- compileExpression x
    xs <- mapM (\(p, b) -> do
      b <- compileExpression b
      compileCase p x b) pats
    return $ embed $ IRSequence $ if length xs == 1 then [head xs] else [createIfSequence xs]
  compileExpression (Constructor c t) = do
    getConstructor (varify c) >>= \case
      Just obj -> return $ IRStructProp (IRVariable obj) (varify c)
      Nothing -> error "Not a constructor!"
  compileExpression (Structure name fields t) = do
    fields' <- mapM (\(x, i) -> (x,) <$> compileExpression i) fields
    return $ IRLamStruct fields'
  compileExpression (Object obj f t) = do
    obj' <- compileExpression obj
    return $ IRStructProp obj' f
  compileExpression (Throw e _) = do
    e' <- compileExpression e
    return $ embed $ IRSequence [IRThrow e']
  compileExpression (Literal i t) = return $ IRLit i
  compileExpression (Async (Sequence stmts)) = do
    stmts' <- concat <$> mapM compileStatement stmts
    let stmts'' = case last stmts' of 
          IRReturn x -> IRSequence stmts'
          x -> IRSequence $ init stmts' ++ [if isStatement x then x else IRReturn x]
    return $ IRCall (IRAsync $ IRLambda [] stmts'') []
  compileExpression (Sequence stmts) = do
    stmts' <- concat <$> mapM compileStatement stmts
    let stmts'' = case last stmts' of 
          IRReturn x -> IRSequence stmts'
          x -> IRSequence $ init stmts' ++ [if isStatement x then x else IRReturn x]
    return $ IRCall (IRLambda [] stmts'') []
  compileExpression (Lambda args body t) = do
    let args' = map (\(n :@ _) -> varify n) args
    body' <- compileExpression body
    return $ IRLambda args' body'
  compileExpression (BinaryOp op e1 e2 t) = do
    e1' <- compileExpression e1
    e2' <- compileExpression e2
    if isValidOperator op 
      then return $ IRBinCall e1' op e2'
      else return $ IRCall (IRVariable (varify op)) [e1', e2']
  compileExpression (UnaryOp op e t) = do
    e' <- compileExpression e
    return $ IRUnaryCall op e'
  compileExpression (Index e1 e2 t) = do
    e1' <- compileExpression e1
    e2' <- compileExpression e2
    return $ IRIndex e1' e2'
  compileExpression (List xs t) = do
    xs' <- mapM compileExpression xs
    return $ IRArray xs'
  compileExpression (Ternary e1 e2 e3 t) = do
    e1' <- compileExpression e1
    e2' <- compileExpression e2
    e3' <- compileExpression e3
    return $ IRTernary e1' e2' e3'
  compileExpression (Async e) = IRAsync <$> compileExpression e
  compileExpression (Await e _) = IRAwait <$> compileExpression e
  compileExpression x = error $ "Not implemented: " ++ show x

  runCompiler :: Monad m => TypeState -> [TypedStatement] -> m [IR]
  runCompiler state stmts = do
    (x, st) <- runStateT (mapM (compileToplevel state) stmts) (M.empty, M.empty)
    return $ concat x