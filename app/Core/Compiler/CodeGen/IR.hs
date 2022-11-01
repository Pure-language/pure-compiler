module Core.Compiler.CodeGen.IR where
  import Core.TypeChecking.Type.AST (Literal)
  import Core.TypeChecking.Type.Pretty ()
  import Data.Char (isLetter, isPrint, ord)
  import Text.Printf (printf)
  
  data IR
    = IRDeclaration String IR
    | IRModification IR IR
    | IRSequence [IR]
    | IRReturn IR
    | IRIf IR IR
    | IRFor String IR IR
    | IRWhile IR IR
    | IRContinue | IRBreak
    | IRIfElse IR IR IR
    | IRExport IR
    | IRImport [String] IR

    | IRCall IR [IR]
    | IRLamStruct [(String, IR)]
    | IRDeref IR
    | IRLambda [String] IR
    | IRVariable String
    | IRBinCall IR String IR
    | IRUnaryCall String IR
    | IRIndex IR IR
    | IRArray [IR]
    | IRTernary IR IR IR
    | IRStructProp IR String
    | IRLit Literal
    | IRIn IR IR
    | IRThrow IR
    | IRAwait IR
    | IRAsync IR
    deriving Show

  isIdent :: Char -> Bool
  isIdent x = isLetter x || x == '_' || x == '$'

  varify :: String -> String
  varify x@(c:_) = (if isIdent c then "" else "$") ++ concatMap (\x -> if isIdent x then [x] else show (ord x)) x
  varify [] = ""

  encodeUnicode16 :: String -> String
  encodeUnicode16 = concatMap escapeChar
    where
      escapeChar c
        | c == '\"' = "\\\""
        | c == '\'' = "\\\'"
        | ' ' <= c && c <= 'z' = [c]
        | isPrint c = [c]
        | otherwise = printf "\\u%04x" (fromEnum c)