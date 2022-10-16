module Core.Compiler.CodeGen.Generation where
  import Core.Compiler.CodeGen.IR
  import Core.TypeChecking.Type.AST (Literal(..))
  import Data.List (intercalate)

  from :: IR -> String
  from (IRSequence asts) = "{" ++ concatMap ((++";") .from) asts ++ "}"
  from (IRDeclaration n v) = "const " ++ n ++ " = " ++ from v ++ ";"
  from (IRModification e v) = from e ++ " = " ++ from v
  from (IRReturn e) = "return " ++ from e ++ ";"
  from (IRIf cond then') = "if (" ++ from cond ++ ") {\n" ++ from then' ++ "}"
  from (IRIfElse cond then' else') = "if (" ++ from cond ++ ") {\n" ++ from then' ++ "} else {\n" ++ from else' ++ "}"

  from (IRCall n args) = from n ++ "(" ++ intercalate "," (map from args) ++ ")"
  from (IRLamStruct fields) = "{" ++ concatMap (\(n, v) -> n ++ ": " ++ from v ++ ", ") fields ++ "}"
  from (IRDeref e) = from e ++ ".value"
  from (IRLambda args body) = 
    "(" ++ intercalate "," args ++ ") => {\n" ++ from body ++ "}"
  from (IRVariable n) = n
  from (IRBinCall e1 op e2) = from e1 ++ " " ++ op ++ " " ++ from e2
  from (IRStructProp e n) = from e ++ "." ++ n
  from (IRLit (I i)) = show i
  from (IRLit (F f)) = show f
  from (IRLit (S s)) = show s
  from (IRLit (C c)) = show c
  from (IRUnaryCall op e) = op ++ from e
  from (IRIndex e i) = from e ++ "[" ++ from i ++ "]"
  from (IRArray e) = "[" ++ intercalate "," (map from e) ++ "]"
  from (IRTernary cond then' else') = from cond ++ " ? " ++ from then' ++ " : " ++ from else'