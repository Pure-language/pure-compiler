module Core.Parser.AST.Literal where
  import Text.Parsec (SourcePos)
  data Located a = a :> (SourcePos, SourcePos)
    deriving (Eq)

  instance Show a => Show (Located a) where
    show (a :> _) = show a

  data Literal
    = I (Located Integer)
    | F (Located Float)
    | S (Located String)
    | C (Located Char)
    deriving (Show, Eq)