{-# LANGUAGE StandaloneDeriving #-}
module Core.TypeChecking.Type.Pretty where
  import Core.TypeChecking.Type.AST
  import Core.Utility.Color
  import Data.List (intercalate)
  
  showStmt :: TypedStatement -> String
  showStmt (Assignment (n :@ t) e)
    = bBlue "let " ++ n ++ " = " ++ show e
  showStmt (Modified n e) 
    = show n ++ " = " ++ show e
  showStmt (If e s1 s2)
    = bBlue "if " ++ show e ++ " then " ++ show s1 ++ " else " ++ show s2
  showStmt (Sequence exprs)
    = "{ " ++ intercalate "; " (map show exprs) ++ " }"
  showStmt (Return e)
    = bBlue "return " ++ show e
  showStmt (Enum (n, _) fields) 
    = bBlue "enum " ++ n ++ " { " ++ intercalate ", " (map (\(x :@ t) -> x ++ ": " ++ show t) fields) ++ "}"
  showStmt (Expression e) = show e
  showStmt (Record (n, _) fields) 
    = bBlue "struct " ++ n ++ " { " ++ intercalate ", " (map (\(x :@ t) -> x ++ ": " ++ show t) fields) ++ "}"
  showStmt (Extern n r)
    = bBlue "extern " ++ n ++ ": " ++ show r
  showStmt (Match e cases)
    = bBlue "match " ++ show e ++ " with " ++ intercalate " | " (map show cases)

  instance Show TypedStatement where
    show = showStmt
    
  showExpr :: TypedExpression -> String
  showExpr (FunctionCall n args _) 
    = show n ++ "(" ++ intercalate ", " (map show args) ++ ")"
  showExpr (Lambda args body _)
    = "(\\" ++ unwords (map (\(x :@ _) -> x) args) ++ " -> " ++ show body ++ ")"
  showExpr (Variable n t) = bold n
  showExpr (Constructor n _) = bGreen $ bold n
  showExpr (Literal l _) = show l
  showExpr (BinaryOp op e1 e2 _) = show e1 ++ " " ++ op ++ " " ++ show e2
  showExpr (UnaryOp op e _) = op ++ show e
  showExpr (List exprs _)
    = "[" ++ intercalate ", " (map show exprs) ++ "]"
  showExpr (Index e i _)
    = show e ++ "[" ++ show i ++ "]"
  showExpr (Structure n fields _)
    = bBlue "struct " ++ n ++ " { " ++ intercalate ", " (map (\(n, f) -> n ++ ": " ++ show f) fields) ++ " }"
  showExpr (Object obj field _)
    = show obj ++ "." ++ field
  showExpr (Ternary e1 e2 e3 _)
    = show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
  showExpr (LetIn (n :@ t) e1 e2 _)
    = bBlue "let " ++ n ++ " = " ++ show e1 ++ bBlue " in " ++ show e2
  showExpr (Reference e _)
    = "&" ++ show e
  showExpr (Unreference e _)
    = "*" ++ show e

  instance Show TypedExpression where
    show = showExpr

  instance Show Literal where
    show (I i) = bYellow $ show i
    show (F f) = bYellow $ show f
    show (S s) = bGreen $ show s
    show (C c) = bGreen $ show c

  deriving instance Show TypedPattern