module Core.Isolation where
  import Core.TypeChecking.Type.AST (TypedStatement(..))

  isolateToplevel :: TypedStatement -> ([TypedStatement], [TypedStatement])
  isolateToplevel z@(Enum _ _) = ([z], [])
  isolateToplevel z = ([], [z])