module Core.Compiler.Type.Free where
  import Core.TypeChecking.Type.AST
  import Data.List (union, (\\))
  import Core.TypeChecking.Type (unannotate)
  
  class Free a where
    free :: a -> [String]
  
  instance Free TypedStatement where
    free (Assignment (name :@ _) e) = free e \\ [name]
    free (Modified n e) = free e \\ free n
    free (If c t e) = free c `union` free t `union` free e
    free (Expression e)  = free e
    free (Return e) = free e
    free (Enum _ _) = []
    free _ = []

  instance Free TypedExpression where
    free (FunctionCall n xs _) = free n `union` free xs
    free (Match e cases) = free e `union` c
      where c = concatMap (\(x,y) -> free y \\ free x) cases
    free (Sequence ss) = free ss
    free (Lambda args body t) = free body \\ args'
      where args' = map (\(x :@ _) -> x) args
    free (Variable s t) = [s]
    free (Literal _ t) = []
    free (BinaryOp s x y t) = free x `union` free y
    free (UnaryOp s x t) = free x
    free (List xs t) = free xs
    free (Index e i t) = free e `union` free i
    free (Structure n fields _) = free $ map snd fields
    free (Object e _ t) = free e
    free (Ternary c t e _) = free c `union` free t `union` free e
    free (LetIn s e body t) = free e `union` (free body \\ [fst $ unannotate s])
    free (Reference c t) = free c
    free (Unreference c t) = free c
    free (Constructor _ t) =[]

  instance Free TypedPattern where
    free (VarP x _) = [x]
    free (LitP _) = []
    free WilP = []
    free (AppP _ xs) = free xs
    free (StructP _ xs) = free $ map snd xs

  instance (Free a, Free b) => Free (a, b) where
    free (x, y) = free x `union` free y

  instance Free a => Free [a] where
    free = foldr (union . free) []