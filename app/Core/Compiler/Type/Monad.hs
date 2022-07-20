module Core.Compiler.Type.Monad where
  import Control.Monad.RWS (MonadState, modify)
  import Data.Map (Map, insert, delete)
  import Core.TypeChecking.Type.Definition (Type)
  
  -- Mapping constructor name to enum type
  type Environment = Map String String

  -- Mapping function to generic arguments
  type GenericMap = Map String [Type]

  data CompilerState = CompilerState Environment GenericMap
    deriving Show

  type MonadCompiler m = MonadState CompilerState m

  addEnv :: MonadCompiler m => String -> String -> m ()
  addEnv name enum = modify  $ \(CompilerState e c) -> CompilerState (insert name enum e) c

  addGeneric :: MonadCompiler m => String -> [Type] -> m ()
  addGeneric name args = modify $ \(CompilerState e c) -> CompilerState e (insert name args c)

  removeGeneric :: MonadCompiler m => String -> m ()
  removeGeneric name = modify $ \(CompilerState e c) -> CompilerState e (delete name c)

  setGenerics :: MonadCompiler m => GenericMap -> m ()
  setGenerics g = modify $ \(CompilerState e _) -> CompilerState e g