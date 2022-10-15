{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Modules.ADT where
  import Core.TypeChecking.Type.AST (TypedStatement(..), Annoted ((:@)), Literal (S))
  import Core.Compiler.CodeGen.IR (IR(..), varify)
  import Core.Compiler.Type
  import Debug.Trace (traceShow, traceShowM)
  import Core.TypeChecking.Type.Definition (Type(TApp, (:->), TRec))
  import Control.Monad.RWS (modify)

  compileData :: MonadCompiler m => TypedStatement -> m IR
  compileData (Enum (n, _) cons) = do
    cons' <- mapM (\(n' :@ t) -> do
              addConstructor (varify n') (varify n)
              case t of
                args :-> _ -> do
                  let args' = zipWith const (["v" ++ show i | i <- [0..]]) args
                  let fields = map (\x -> (x, IRVariable x)) args'
                  let fields' = IRLamStruct (("type", IRLit (S n')) : fields)
                  return (varify n', IRLambda args' (IRReturn fields'))
                _ -> return (varify n', IRLamStruct [("type", IRLit (S n'))])) cons
    return $ IRDeclaration (varify n) (IRLamStruct cons')
  compileData _ = error "compileData: not a data"