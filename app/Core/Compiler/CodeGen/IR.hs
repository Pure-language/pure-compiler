module Core.Compiler.CodeGen.IR where
  type CType = String
  data CppAST 
    = CDeclaration (String, CType) CppAST
    | CModification CppAST CppAST
    | CIf CppAST CppAST CppAST
    | CSequence [CppAST]
    | CReturn CppAST
    | CStruct String [(String, CType)]
    | CTemplate [CType] CppAST
    | CEnum String [String]
    deriving Show