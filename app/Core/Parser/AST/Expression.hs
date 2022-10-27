module Core.Parser.AST.Expression where
  import Core.Parser.AST.Literal (Literal, Located)
  import Text.Parsec (SourcePos)

  data Declaration
    = Id String [String]
    | Arrow [Declaration] [Declaration] Declaration
    | StructE [(String, Declaration)]
    | AppE Declaration Declaration
    | StrE | IntE | FloatE | CharE | VoidE | BoolE
    | Ref Declaration
    deriving (Show, Eq)

  data Statement
    = Assignment (Annoted String) (Located Expression)
    | Modified (Located Expression) (Located Expression)
    | If (Located Expression) (Located Expression) (Located Expression)
    | Expression Expression
    | Return (Located Expression)
    | Enum String [Declaration] [(String, Maybe [Declaration])]
    | Extern [Declaration] String Declaration
    | Instance [(String, [String])] String Declaration [(String, Located Expression)] Bool
    | Class [Declaration] String [(String, Declaration)]
    | Record String [Declaration] [(String, Declaration)]
    | Import [String]
    | Public (Located Statement)
    | For String (Located Expression) (Located Expression)
    | While (Located Expression) (Located Expression)
    | Continue | Break
    | Macro String [String] (Located Expression)
    deriving (Show, Eq)

  data Annoted a = a :@ Maybe Declaration
    deriving (Show, Eq)

  data Expression
    = FunctionCall (Located Expression) [Located Expression]
    | Throw (Located Expression)
    | Lambda [Declaration] [Annoted String] (Located Expression)
    | Sequence [Located Statement]
    | Variable String [Declaration]
    | Literal Literal
    | BinaryOp String (Located Expression) (Located Expression)
    | UnaryOp String (Located Expression)
    | List [Located Expression]
    | Index (Located Expression) (Located Expression)
    | Match (Located Expression) [(Located Expression, Located Expression)]
    | LetIn (Annoted String) (Located Expression) (Located Expression)
    | Structure String [(String, Located Expression)]
    | Object (Located Expression) String
    | Ternary (Located Expression) (Located Expression) (Located Expression)
    | Reference (Located Expression)
    | Unreference (Located Expression)
    | Cast (Located Expression) [Declaration]
    deriving (Show, Eq)