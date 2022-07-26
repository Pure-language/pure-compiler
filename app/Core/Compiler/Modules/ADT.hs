module Core.Compiler.Modules.ADT where
  import Core.TypeChecking.Type.AST (TypedStatement(..), Annoted ((:@)))
  import Core.Compiler.CodeGen.IR (CppAST(..), StructField (SType, SLambda))
  import Core.Compiler.Type (createTemplate, fromType, MonadCompiler, addEnv, getEnv)
  import Debug.Trace (traceShow)
  import Core.TypeChecking.Type.Definition (Type(TApp, (:->)))

  createEnum :: TypedStatement -> CppAST
  createEnum (Enum (n, _) fields) = do
    let fields' = map (\(n :@ _) -> n) fields
    CEnum (n ++ "E") fields'
  createEnum _ = undefined

  createStructure :: MonadCompiler  m => TypedStatement -> m CppAST
  createStructure (Enum (name, ty) fields) = do
    let t = createTemplate ty
    let n' = fromType ty
    let values = concatMap (\(n :@ t) -> case t of
                  t :-> _ -> zip (["v" ++ show i | i <- [0..]]) t
                  _ -> []) fields
    let fields'   = map (\(n, t) -> SType n (fromType t)) values
    let enumField = SType "TYPE" $ "enum " ++ name ++ "E"
    methods <- mapM (\(n :@ t) -> case t of
        t :-> _ -> do
          addEnv n True name
          let args = zip (["v" ++ show i | i <- [0..]]) (map fromType t)
          let args' = map fst args
          let struct = CLamStruct n' $ ("TYPE", CVariable n) : zip args' (map CVariable args')
          return $ SLambda n' n args struct
        _ -> do
          addEnv n False name
          let struct = CLamStruct n' [("TYPE", CVariable n)]
          return $ SLambda n' n [] struct) fields
    return $ CStruct (name ++ "T") (enumField : fields' ++ methods)
  createStructure _ = undefined