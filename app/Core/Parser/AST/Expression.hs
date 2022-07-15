module Core.Parser.AST.Expression where
  import Core.Parser.AST.Literal (Literal, Located)
  import Text.Parsec (SourcePos)
  
  data Declaration
    = Generic String
    | Arrow [Declaration] Declaration
    | Array Declaration
    | StrE | IntE | FloatE | CharE
    deriving (Show, Eq)

  data Statement
    = Assignment String (Located Expression)
    | If (Located Expression) (Located Statement) (Located Statement)
    | Sequence [Located Statement]
    | Expression Expression
    | Return (Located Expression)
    | Declaration String Declaration
    deriving (Show, Eq)

  data Expression
    = FunctionCall (Located Expression) [Located Expression]
    | Lambda [String] (Located Statement)
    | Variable String
    | Literal Literal
    | BinaryOp String (Located Expression) (Located Expression)
    | UnaryOp String (Located Expression)
    | List [Located Expression]
    | Index (Located Expression) (Located Expression)
    deriving (Show, Eq)