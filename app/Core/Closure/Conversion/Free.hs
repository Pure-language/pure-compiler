module Core.Closure.Conversion.Free where
  import Data.Set (Set, (\\), fromList, unions, singleton, empty, union)
  import Core.Parser.AST

  class Free a where
    free :: a -> Set String
  
  instance Free a => Free (Located a) where
    free (a :> _) = free a
  
  instance Free a => Free (Annoted a) where
    free (a :@ _) = free a

  instance Free Expression where
    free (Lambda args body) = free body \\ fromList (map (\(x :@ _) -> x) args)
    free (FunctionCall n xs) = unions $ free n : map free xs
    free (Variable x) = singleton x
    free (Literal _) = empty
    free (BinaryOp _ x y) = unions [free x, free y]
    free (UnaryOp _ x) = free x
    free (List xs) = unions $ map free xs
    free (Index n i) = unions [free n, free i]

  instance Free Statement where
    free (Assignment (x :@ _) e) = free e \\ singleton x
    free (If e s1 s2) = unions [free e, free s1, free s2]
    free (Sequence stmts) = foldl (\acc stmt -> acc `union` free stmt) empty stmts
    free (Expression e) = free e
    free (Return e) = free e