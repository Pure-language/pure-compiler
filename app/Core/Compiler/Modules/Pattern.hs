module Core.Compiler.Modules.Pattern where
  import Core.TypeChecking.Type.AST (TypedPattern(..), Literal(..))
  import Core.Compiler.CodeGen.IR (CppAST(..), isStatement)
  import Core.Compiler.Type (fromType, MonadCompiler, getEnv)
  import qualified Data.Map as M
  import Prelude hiding (and)
  import Data.Maybe (fromJust, isJust)
  
  and :: CppAST -> CppAST -> CppAST
  and l = CBinCall l "&&"

  findPattern :: TypedPattern -> CppAST -> [(Maybe CppAST, Maybe CppAST)]
  findPattern (LitP l) e    = [(Nothing, Just $ CBinCall e "==" (Lit l))]
  findPattern WilP e        = [(Nothing, Nothing)]
  findPattern (VarP v t) e  = [(Just $ CDeclaration (v, fromType t) e, Nothing)]
  findPattern (AppP n xs) e = 
    concat $ [(Nothing, Just $ CBinCall (CStructProp e "TYPE") "==" (CVariable n))] : zipWith (\x y -> findPattern y (CStructProp e x))
    (["v" ++ show i | i <- [0..]]) xs

  compileCase :: MonadCompiler m => TypedPattern -> m (CppAST -> [CppAST] -> CppAST)
  compileCase (VarP n t) = do
    getEnv >>= \e -> case M.lookup n e of
      Just _ -> return $ \x body ->
        let cond = CBinCall (CStructProp x "TYPE") "==" (CVariable n)
          in CIf cond $ CSequence body
      Nothing -> return $ \x body -> CSequence [CDeclaration (n, fromType t) x, CSequence body]
  compileCase (AppP n args) = do
    return $ \x b -> do
      let args' = concat $ zipWith (\arg v -> findPattern arg (CStructProp x v)) args (["v" ++ show i | i <- [0..]])
      let lets   = map (fromJust . fst) $ filter (isJust . fst) args'
      let cs = map (fromJust . snd) $ filter (isJust . snd) args'
      let cond  = CBinCall (CStructProp x "TYPE") "==" (CVariable n)
          conds = case cs of
                    (c:cs) -> cond `and` foldl and c cs
                    _ -> cond
        in CIf conds $ CSequence $ lets ++ b
  compileCase WilP = do
    return $ \x b -> CSequence b
  compileCase (LitP l) = do
    return $ \x b ->
      let cond = CBinCall x "===" (Lit l)
        in CIf cond $ CSequence b