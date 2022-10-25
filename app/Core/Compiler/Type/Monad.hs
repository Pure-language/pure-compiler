module Core.Compiler.Type.Monad where
  import Control.Monad.RWS (MonadState (get, put), modify, gets)
  import Data.Map (Map, insert, delete, empty)
  import Core.TypeChecking.Type.Definition (Type)
  import Core.Compiler.CodeGen (IR)
  import qualified Data.Map as M
  import Core.TypeChecking.Type.AST
  import Control.Arrow (Arrow(first))
  import Data.Functor
  
  type Constructors = Map String String
  type MonadCompiler m = (MonadState (Constructors, Map String TypedStatement) m)

  addConstructor :: MonadCompiler m => String -> String -> m ()
  addConstructor name constructor = modify . first $ insert name constructor

  removeConstructor :: MonadCompiler m => String -> m ()
  removeConstructor name = modify . first $ delete name

  getConstructor :: MonadCompiler m => String -> m (Maybe String)
  getConstructor e = gets fst <&> M.lookup e