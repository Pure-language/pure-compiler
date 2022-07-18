module Core.Parser.AST.Expression where
  import Core.Parser.AST.Literal (Literal, Located)
  import Text.Parsec (SourcePos)
  
  data Declaration
    = Generic String
    | Arrow [Declaration] Declaration
    | Array Declaration
    | StructE [(String, Declaration)]
    | AppE String [Declaration]
    | StrE | IntE | FloatE | CharE
    | Ref Declaration
    deriving (Show, Eq)

  data Statement
    = Assignment (Annoted String) (Located Expression)
    | Modified (Located Expression) (Located Expression)
    | If (Located Expression) (Located Statement) (Located Statement)
    | Sequence [Located Statement]
    | Expression Expression
    | Return (Located Expression)
    | Enum String [(String, Maybe [Declaration])]
    deriving (Show, Eq)

  data Annoted a = a :@ Maybe Declaration
    deriving (Show, Eq)

  data Expression
    = FunctionCall (Located Expression) [Located Expression]
    | Lambda [Annoted String] (Located Statement)
    | Variable String
    | Literal Literal
    | BinaryOp String (Located Expression) (Located Expression)
    | UnaryOp String (Located Expression)
    | List [Located Expression]
    | Index (Located Expression) (Located Expression)
    | Structure [(String, Located Expression)]
    | Object (Located Expression) String
    | Ternary (Located Expression) (Located Expression) (Located Expression)
    | Reference (Located Expression)
    | Unreference (Located Expression)
    deriving (Show, Eq)