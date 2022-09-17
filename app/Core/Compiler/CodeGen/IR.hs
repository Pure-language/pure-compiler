module Core.Compiler.CodeGen.IR where
  import Core.TypeChecking.Type.AST (Literal)
  import Core.TypeChecking.Type.Pretty ()
  
  type CType = String
  data CppAST
    = CDeclaration (String, CType) CppAST
    | CModification CppAST CppAST
    | CSequence [CppAST]
    | CReturn CppAST
    | CStruct String [StructField]
    | CEnum String [String]
    | CFunction CType String [(String, CType)] CppAST
    | CIf CppAST CppAST
    | CIfElse CppAST CppAST CppAST
    | CExtern String [CType] CType

    | CCall CppAST [CppAST] [CType]
    | CLamStruct [(String, CppAST)]
    | CRef CppAST
    | CDeref CppAST
    | CLambda [String] [(String, CType)] CppAST
    | CVariable String
    | CBinCall CppAST String CppAST
    | CStructProp CppAST String
    | CCast CType CppAST
    | Lit Literal
    deriving Show

  data StructField
    = SType String CType
    deriving Show

  isStatement :: CppAST -> Bool
  isStatement (CDeclaration _ _) = True
  isStatement (CModification _ _) = True
  isStatement (CSequence _) = True
  isStatement (CReturn _) = True
  isStatement (CStruct _ _) = True
  isStatement (CEnum _ _) = True
  isStatement (CIf _ _) = True
  isStatement CIfElse {} = True
  isStatement _ = False
