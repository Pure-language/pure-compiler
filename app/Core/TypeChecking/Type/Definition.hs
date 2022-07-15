module Core.TypeChecking.Type.Definition where
  import Data.Map (Map)
  
  data Type
    = TVar Int
    | [Type] :-> Type
    | Int | String | Float | Bool | Char
    | ListT Type
    deriving (Eq, Ord)

  type TypeEnv = Map String Scheme

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord, Show)

  instance Show Type where
    show (TVar i) = "t" ++ show i
    show (t :-> u) = "(" ++ show t ++ " -> " ++ show u ++ ")"
    show Int = "Int"
    show String = "String"
    show Float = "Float"
    show Bool = "Bool"
    show Char = "Char"
    show (ListT t) = "[" ++ show t ++ "]"