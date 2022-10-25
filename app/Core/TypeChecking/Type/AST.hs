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
    | Record (String, Type) [Annoted String]
    | Extern String Type
    | Match TypedExpression [(TypedPattern, TypedStatement)]
    | For (String, Type) TypedExpression TypedStatement
    | While TypedExpression TypedStatement
    | Continue | Break
    | Public TypedStatement
    | Import TypedExpression String
    deriving Eq

  data TypedPattern
    = VarP String Type
    | LitP Literal
    | WilP
    | AppP String [TypedPattern]
    | StructP String [(String, TypedPattern)]
    deriving Eq

  data Annoted a = a :@ Type
    deriving (Show, Eq)

  data TypedExpression
    = FunctionCall TypedExpression [TypedExpression] Type
    | Lambda [Annoted String] TypedStatement Type    
    | Variable String Type
    | Constructor String Type
    | Literal Literal Type
    | BinaryOp String TypedExpression TypedExpression Type
    | UnaryOp String TypedExpression Type
    | List [TypedExpression] Type
    | Index TypedExpression TypedExpression Type
    | Structure String [(String, TypedExpression)] Type
    | Object TypedExpression String Type
    | Ternary TypedExpression TypedExpression TypedExpression Type
    | LetIn (Annoted String) TypedExpression TypedExpression Type
    | Reference TypedExpression Type
    | Unreference TypedExpression Type
    deriving Eq

  data Literal
    = S String
    | I Integer
    | F Float 
    | C Char
    deriving Eq