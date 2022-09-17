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
    | Extern String [Type] Type
    | Match TypedExpression [(TypedPattern, TypedStatement)]
    deriving Eq

  data TypedPattern
    = VarP String Type
    | LitP Literal
    | WilP
    | AppP String [TypedPattern]
    deriving Eq

  data Annoted a = a :@ Type
    deriving (Show, Eq)

  data TypedExpression
    = FunctionCall TypedExpression [TypedExpression] Type
    | Lambda [Annoted String] TypedStatement Type    
    | Variable String Type
    | Constructor String Type
    | Literal Literal
    | BinaryOp String TypedExpression TypedExpression
    | UnaryOp String TypedExpression
    | List [TypedExpression]
    | Index TypedExpression TypedExpression
    | Structure [(String, TypedExpression)] Type
    | Object TypedExpression String
    | Ternary TypedExpression TypedExpression TypedExpression
    | LetIn (Annoted String) TypedExpression TypedExpression Type
    | Reference TypedExpression
    | Unreference TypedExpression
    deriving Eq

  data Literal
    = S String
    | I Integer
    | F Float 
    | C Char
    deriving Eq