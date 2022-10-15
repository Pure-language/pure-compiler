module Core.TypeChecking.Type.Definition where
  import Data.Map (Map)
  import Data.List (intercalate)
  
  data Type
    = TVar Int
    | [Type] :-> Type
    | [Class] :=> Type
    | Int | Float | Bool | Char | Void
    | ListT Type | TRec [(String, Type)]
    | TId String
    | TApp Type [Type]
    | RefT Type
    deriving (Eq, Ord)
    
  data Class = IsIn String [Type]
    deriving (Eq, Ord)
  type InstanceName = String
  type Instance = ([Class], (InstanceName, [Class]))
  type Instances = [Instance]

  data Constructor
    = Fun Scheme
    | Struct Scheme

  type TypeEnv = Map String Scheme
  type ConsEnv = Map String Scheme
  type Env = (TypeEnv, ConsEnv)

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord, Show)

  instance Show Class where
    show (IsIn c ts) = c ++ " " ++ unwords (map show ts)

  instance Show Type where
    show (TVar i) = "t" ++ show i
    show (t :-> u) = "fun(" ++ intercalate ", " (map show t) ++ ") -> " ++ show u
    show (ListT Char) = "str"
    show Int = "int"
    show Void = "void"
    show Float = "float"
    show Bool = "bool"
    show Char = "char"
    show (ListT t) = "[" ++ show t ++ "]"
    show (TRec fs) = "struct { " ++ intercalate ", " (map (\(n, t) -> n ++ " : " ++ show t) fs) ++ " }"
    show (RefT t) = "ref " ++ show t
    show (TId s) = s
    show (TApp s args) = show s ++ ("<" ++ intercalate ", " (map show args) ++ ">")
    show (cs :=> t) = intercalate ", " (map show cs) ++ " => " ++ show t