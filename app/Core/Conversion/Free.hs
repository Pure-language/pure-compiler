module Core.Conversion.Free where
  import qualified Data.Set as S
  import Core.TypeChecking.Type.AST
  import Core.TypeChecking.Type.Definition
  
  class Free a where
    free :: a -> S.Set (String, Type)

  instance Free a => Free [a] where
    free = S.unions . map free

  unannotate :: Annoted a -> (a, Type)
  unannotate (x :@ t) = (x, t)
  
  instance Free TypedExpression where
    free (Variable x t) = S.singleton (x, t)
    free (FunctionCall n args _) = free n `S.union` free args
    free (Lambda args body t) = 
      free body S.\\ S.fromList (map unannotate args)
    free (BinaryOp _ a b) = free a `S.union` free b
    free (UnaryOp _ a) = free a
    free (List exprs) = free exprs
    free (Index a b) = free a `S.union` free b
    free (Structure fields _) = free (map snd fields)
    free (Object e _) = free e
    free (Ternary a b c) = free a `S.union` free b `S.union` free c
    free (Reference e) = free e
    free (Unreference e) = free e
    free _ = S.empty

  instance Free TypedPattern where
    free (VarP x t) = S.singleton (x, t)
    free (AppP n args) = free args
    free _ = S.empty
  
  instance Free TypedStatement where
    free (Assignment n e) = free e S.\\ S.singleton (unannotate n)
    free (Modified n e) = free e S.\\ free n
    free (If e s1 s2) = free e `S.union` free s1 `S.union` free s2
    free (Sequence exprs) = free exprs
    free (Expression expr) = free expr
    free (Return expr) = free expr
    free (Match expr cases) 
      = free expr `S.union` S.unions (map (\(x, y) -> free y S.\\ free x) cases)
    free _ = S.empty