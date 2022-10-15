{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Compiler where
  import qualified Data.Map as M
  import Data.List (intercalate, union, (\\))
  import Core.TypeChecking.Type.Definition
  import Core.TypeChecking.Type.AST
  import Core.Compiler.CodeGen (IR(..), varify)
  import Control.Monad.RWS (gets, modify)
  import Control.Monad.State (StateT(runStateT), evalStateT)
  import Core.Compiler.Modules.ADT (compileData)
  import Core.Compiler.Modules.Pattern (compileCase)
  import Core.TypeChecking.Type.Pretty ()
  import Control.Arrow (Arrow(second))
  import Debug.Trace (traceShowM)
  import Core.Compiler.Type (MonadCompiler, getConstructor)
  
  compileToplevel :: MonadCompiler m => TypedStatement -> m [IR]
  compileToplevel (Assignment (name :@ ty) e) = do
    e' <- compileExpression e
    return [IRDeclaration (varify name) e']
  compileToplevel z@(Enum (n, ty) fields) = (:[]) <$> compileData z
  compileToplevel (Extern n ret) = return []
  compileToplevel (Record (name, ty) fields) = return []
  compileToplevel x = error $ "Not implemented: " ++ show x

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
    s1' <- compileStatement s1
    s2' <- compileStatement s2
    return [IRIfElse e' (IRSequence s1') (IRSequence s2')]
  compileStatement (Sequence stmts) = do
    stmts' <- mapM compileStatement stmts
    return $ concat stmts'
  compileStatement (Expression e)
    = (:[]) <$> compileExpression e
  compileStatement (Return e)
    = (:[]) . IRReturn <$> compileExpression e
  compileStatement (Match x pats) = do
    x <- compileExpression x
    xs <- mapM (\(p, b) -> do
      b <- compileStatement b
      compileCase p x b) pats
    return $ [if length xs == 1 then head xs else foldl1 (\acc (IRIf cond then') -> IRIfElse cond then' acc) xs]
  compileStatement x = error $ "Not implemented: " ++ show x

  compileExpression :: MonadCompiler m => TypedExpression -> m IR
  compileExpression (Variable "void" _) = return (IRVariable "null")
  compileExpression (Variable name t) = return $ IRVariable (varify name)
  --compileExpression (FunctionCall (Variable n) args ty) = do
  --  args' <- mapM compileExpression args
  --  ty' <- gets genericMap >>= \e -> return (case M.lookup n e of
  --    Just t -> match t ty
  --    Nothing -> [])
  --  return $ CCall (CVariable n) args' ty'
  compileExpression (FunctionCall call args ty) = do
    call' <- compileExpression call
    args' <- mapM compileExpression args
    return $ IRCall call' args'
  compileExpression (Reference c t) = IRLamStruct . (:[]) . ("value",) <$> compileExpression c
  compileExpression (Unreference c t) = IRDeref <$> compileExpression c
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
  compileExpression (Literal i t) = return $ IRLit i
  compileExpression (Lambda args body t) = do
    let args' = map (\(n :@ _) -> varify n) args
    body' <- compileStatement body
    return $ IRLambda args' (if length body' == 1 then head body' else IRSequence body')
  compileExpression (BinaryOp op e1 e2 t) = do
    e1' <- compileExpression e1
    e2' <- compileExpression e2
    return $ IRBinCall e1' op e2'
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
  compileExpression x = error $ "Not implemented: " ++ show x

  runCompiler :: Monad m => [TypedStatement] -> m [IR]
  runCompiler stmts = do
    (x, st) <- runStateT (mapM compileToplevel stmts) M.empty
    return $ concat x