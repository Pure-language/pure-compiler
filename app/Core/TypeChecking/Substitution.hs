module Core.TypeChecking.Substitution where
  import Data.Map (Map)
  import Core.TypeChecking.Type.Definition (Type)
  import qualified Data.Set as S
  import qualified Data.Map as M
  
  type Substitution = Map Int Type

  class Types a where
    free  :: a -> S.Set Int
    apply :: Substitution -> a -> a