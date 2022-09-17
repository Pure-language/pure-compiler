module Core.Compiler.CodeGen.Generation where
  import Core.Compiler.CodeGen.IR
  import Core.TypeChecking.Type.AST (Literal(..))
  import Data.List (intercalate)

  from :: CppAST -> String
  from (CSequence asts) = "{" ++ concatMap ((++";") .from) asts ++ "}"
  from (CDeclaration (n, t) v) = t ++ " " ++ n ++ " = " ++ from v ++ ";"
  from (CModification e v) = "*" ++ from e ++ " = " ++ from v
  from (CReturn e) = "return " ++ from e ++ ";"
  from (CStruct n fields) = "struct " ++ n ++ " {\n" ++ concatMap (\(SType n t) -> "  " ++ t ++ " " ++ n ++ ";\n") fields ++ "};"
  from (CEnum n fields) = "enum " ++ n ++ " {\n" ++ concatMap (\n -> "  " ++ n ++ ",\n") fields ++ "};"
  from (CFunction ret name args body) =
    ret ++ " " ++ name ++ "(" ++ intercalate "," (map (\(n, t) -> t ++ " " ++ n) args) ++ ") {\n" ++ from body ++ "}"
  from (CIf cond then') = "if (" ++ from cond ++ ") {\n" ++ from then' ++ "}"
  from (CIfElse cond then' else') = "if (" ++ from cond ++ ") {\n" ++ from then' ++ "} else {\n" ++ from else' ++ "}"
  from (CExtern name args ret) = "extern " ++ ret ++ " " ++ name ++ "(" ++ intercalate "," args ++ ");"

  from (CCall n args _) = from n ++ "(" ++ intercalate "," (map from args) ++ ")"
  from (CLamStruct fields) = "{" ++ concatMap (\(n, v) -> "." ++ n ++ " = " ++ from v ++ ", ") fields ++ "}"
  from (CRef e) = "&" ++ from e
  from (CDeref e) = "(*" ++ from e ++ ")"
  from (CLambda _ _ _) = error "Should not happen"
  from (CVariable n) = n
  from (CBinCall e1 op e2) = from e1 ++ " " ++ op ++ " " ++ from e2
  from (CStructProp e n) = from e ++ "." ++ n
  from (CCast t e) = "(" ++ t ++ ")" ++ from e
  from (Lit (I i)) = show i
  from (Lit (F f)) = show f
  from (Lit (S s)) = show s
  from (Lit (C c)) = show c