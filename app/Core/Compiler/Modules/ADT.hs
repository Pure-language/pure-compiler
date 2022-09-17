module Core.Compiler.Modules.ADT where
  import Core.TypeChecking.Type.AST (TypedStatement(..), Annoted ((:@)))
  import Core.Compiler.CodeGen.IR (CppAST(..), StructField (SType))
  import Core.Compiler.Type
  import Debug.Trace (traceShow, traceShowM)
  import Core.TypeChecking.Type.Definition (Type(TApp, (:->)))
  import Control.Monad.RWS (modify)
  
  createEnum :: TypedStatement -> CppAST
  createEnum (Enum (n, _) fields) = do
    let fields' = map (\(n :@ _) -> n ++ "C") fields
    CEnum (n ++ "E") fields'
  createEnum _ = undefined

  createStructure :: MonadCompiler  m => TypedStatement -> m [CppAST]
  createStructure (Enum (name, ty) fields) = do
    let t = createTemplate ty
    traceShowM ("cons", name)
    modify $ \s -> s { structures = (name, ty) : structures s }
    
    n' <- fromType ty
    let values = concatMap (\(n :@ t) -> case t of
                  t :-> _ -> zip (["v" ++ show i | i <- [0..]]) t
                  _ -> []) fields
    fields' <- mapM (\(n, t) -> SType n <$> fromType t) values
    let enumField = SType "TYPE" $ "enum " ++ name ++ "E"
    methods <- mapM (\(n :@ t) -> case t of
        t :-> _ -> do
          addEnv n True name
          traceShowM n
          t <- mapM fromType t
          let args = zip (["v" ++ show i | i <- [0..]]) t
          let args' = map fst args
          let struct = CCast n' $ CLamStruct $ ("TYPE", CVariable (n ++ "C")) : zip args' (map CVariable args')
          return $ CFunction n' n args (CReturn struct)
        _ -> do
          addEnv n False name
          let struct = CCast n' $ CLamStruct [("TYPE", CVariable (n ++ "C"))]
          return $ CFunction n' n [] (CReturn struct)) fields
    return $ CStruct name (enumField : fields') : methods
  createStructure _ = undefined