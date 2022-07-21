module Core.Compiler.Type.Monad where
  import Control.Monad.RWS (MonadState (get, put), modify)
  import Data.Map (Map, insert, delete)
  import Core.TypeChecking.Type.Definition (Type)
  import Core.Compiler.CodeGen (CppAST)
  
  -- Mapping constructor name to enum type
  type Environment = Map String String

  -- Mapping function to generic arguments
  type GenericMap = Map String [Type]

  data CompilerState = CompilerState {
    environment :: Environment,
    genericMap :: GenericMap,
    toplevel :: [String],
    structures :: [CppAST],
    counter :: Int
  } deriving Show

  type MonadCompiler m = MonadState CompilerState m

  addEnv :: MonadCompiler m => String -> String -> m ()
  addEnv name enum = modify  $ \s -> s { environment = insert name enum (environment s) }

  addGeneric :: MonadCompiler m => String -> [Type] -> m ()
  addGeneric name args = modify $ \s -> s { genericMap = insert name args (genericMap s) }

  removeGeneric :: MonadCompiler m => String -> m ()
  removeGeneric name = modify $ \s -> s { genericMap = delete name (genericMap s) }

  setGenerics :: MonadCompiler m => GenericMap -> m ()
  setGenerics g = modify $ \s -> s { genericMap = g }

  addToplevel :: MonadCompiler m => String -> m ()
  addToplevel name = modify $ \s -> s { toplevel = name : toplevel s }

  addStructure :: MonadCompiler m => CppAST -> m ()
  addStructure ast = modify $ \s -> s { structures = ast : structures s }

  inc :: MonadCompiler m => m Int
  inc = do
    s <- get
    put $ s { counter = counter s + 1 }
    return $ counter s