module Core.Compiler.CodeGen.Generation where
  import Core.Compiler.CodeGen.IR
  import Core.TypeChecking.Type.AST (Literal(..))
  import Data.List (intercalate)

  isStatement :: IR -> Bool
  isStatement (IRDeclaration _ _) = True
  isStatement (IRModification _ _) = True
  isStatement (IRSequence _) = True
  isStatement (IRReturn _) = True
  isStatement (IRIf _ _) = True
  isStatement IRFor {} = True
  isStatement (IRWhile _ _) = True
  isStatement IRContinue = True
  isStatement IRBreak = True
  isStatement IRIfElse {} = True
  isStatement (IRExport _) = True
  isStatement (IRImport _ _) = True
  isStatement (IRThrow _) = True
  isStatement (IRAwait _) = True
  isStatement _ = False

  from :: IR -> String
  from (IRSequence asts) = "{" ++ concatMap ((++";") .from) asts ++ "}"
  from (IRDeclaration n v) = "const " ++ n ++ " = " ++ from v ++ ";"
  from (IRModification e v) = from e ++ " = " ++ from v
  from (IRReturn e) = "return " ++ from e ++ ";"
  from (IRIf cond then') = "if (" ++ from cond ++ ") {\n" ++ from then' ++ "}"
  from (IRIfElse cond then' else') = "if (" ++ from cond ++ ") {\n" ++ from then' ++ "} else {\n" ++ from else' ++ "}"
  from (IRFor n expr body) = "for(const " ++ n ++ " of " ++ from expr ++ ")" ++ from body
  from (IRWhile cond body) = "while (" ++ from cond ++ ")" ++ from body
  from IRContinue = "continue;"
  from IRBreak = "break;"
  from (IRImport names from') = "const { " ++ intercalate ", " names ++ " } = " ++ from from' ++ ";"
  
  from (IRCall l@(IRLambda _ _) args') = "(" ++ from l ++ ")(" ++ intercalate ", " (map from args') ++ ")"
  from (IRCall l@(IRAsync _) args') = "(" ++ from l ++ ")(" ++ intercalate ", " (map from args') ++ ")"
  from (IRCall n args) = from n ++ "(" ++ intercalate "," (map from args) ++ ")"
  from (IRLamStruct fields) = "{" ++ concatMap (\(n, v) -> n ++ ": " ++ from v ++ ", ") fields ++ "}"
  from (IRDeref e) = from e ++ ".value"
  from (IRLambda args s@(IRLamStruct _)) = "(" ++ intercalate ", " args ++ ") => (" ++ from s ++ ")"
  from (IRLambda args body) = 
    "(" ++ intercalate "," args ++ ") => " ++ from body
  from (IRVariable n) = n
  from (IRBinCall e1 op e2) = "(" ++ from e1 ++ ") " ++ op ++ " (" ++ from e2 ++ ")"
  from (IRStructProp e n) = from e ++ "." ++ n
  from (IRLit (I i)) = show i
  from (IRLit (F f)) = show f
  from (IRLit (S s)) = show s
  from (IRLit (C c)) = show c
  from (IRUnaryCall op e) = op ++ from e
  from (IRIndex e i) = from e ++ "[" ++ from i ++ "]"
  from (IRArray e) = "[" ++ intercalate "," (map from e) ++ "]"
  from (IRTernary cond then' else') = from cond ++ " ? " ++ from then' ++ " : " ++ from else'
  from (IRExport e) = "export " ++ from e ++ ";"
  from (IRAwait e) = "await " ++ from e
  from (IRIn e1 e2) = from e1 ++ " in " ++ from e2
  from (IRThrow e) = "throw " ++ from e
  from (IRAsync e) = "async " ++ from e