module Core.TypeChecking.Type.AST where
  import Core.TypeChecking.Type.Definition (Type)
  data TypedStatement
    = Assignment (Annoted String) TypedExpression
    | Modified TypedExpression TypedExpression
    | If TypedExpression TypedStatement TypedStatement    
    | Sequence [TypedStatement]
    | Expression TypedExpression
    | Return TypedExpression
    | Enum (String, Type) [Annoted String]
    deriving (Show, Eq)

  data TypedPattern
    = VarP String Type
    | LitP Literal
    | WilP
    | AppP String [TypedPattern]
    deriving (Eq, Show)

  data Annoted a = a :@ Type
    deriving (Show, Eq)

  data TypedExpression
    = FunctionCall TypedExpression [TypedExpression] [Type]
    | Lambda [Annoted String] TypedStatement    
    | Variable String
    | Constructor String
    | Literal Literal
    | BinaryOp String TypedExpression TypedExpression
    | UnaryOp String TypedExpression
    | List [TypedExpression]
    | Index TypedExpression TypedExpression
    | Structure [(String, TypedExpression)]
    | Object TypedExpression String
    | Ternary TypedExpression TypedExpression TypedExpression
    | Reference TypedExpression
    | Unreference TypedExpression
    | Match TypedExpression [(TypedPattern, TypedStatement)]
    deriving (Show, Eq)

  data Literal
    = S String
    | I Integer
    | F Float 
    | C Char
    deriving (Show, Eq)