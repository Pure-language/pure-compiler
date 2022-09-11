module Core.TypeChecking.Type.Definition where
  import Data.Map (Map)
  import Data.List (intercalate)
  
  data Type
    = TVar Int
    | [Type] :-> Type
    | Int | String | Float | Bool | Char
    | ListT Type | TRec [(String, Type)]
    | TId String
    | TApp Type [Type]
    | RefT Type
    deriving (Eq, Ord)

  type TypeEnv = Map String Scheme
  type Env = (TypeEnv, TypeEnv)

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord, Show)

  instance Show Type where
    show (TVar i) = "t" ++ show i
    show (t :-> u) = "fun(" ++ intercalate ", " (map show t) ++ ") -> " ++ show u
    show (ListT Char) = "String"
    show Int = "Int"
    show String = "String"
    show Float = "Float"
    show Bool = "Bool"
    show Char = "Char"
    show (ListT t) = "[" ++ show t ++ "]"
    show (TRec fs) = "struct {" ++ intercalate ", " (map (\(n, t) -> n ++ " : " ++ show t) fs) ++ "}"
    show (RefT t) = "ref " ++ show t
    show (TId s) = s
    show (TApp s args) = show s ++ (if null args then "" else "<" ++ intercalate ", " (map show args) ++ ">")