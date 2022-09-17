{-# LANGUAGE TupleSections #-}
module Core.Compiler.Modules.Pattern where
  import Core.TypeChecking.Type.AST (TypedPattern(..), Literal(..))
  import Core.Compiler.CodeGen.IR (CppAST(..), isStatement)
  import Core.Compiler.Type (fromType, MonadCompiler, getEnv)
  import qualified Data.Map as M
  import Prelude hiding (and)
  import Data.Maybe (fromJust, isJust)
  import Control.Monad (zipWithM)

  and :: CppAST -> CppAST -> CppAST
  and l = CBinCall l "&&"

  findPattern :: MonadCompiler m => TypedPattern -> m (CppAST -> [(Maybe CppAST, Maybe CppAST)])
  findPattern (LitP l)    = return $ \e -> [(Nothing, Just $ CBinCall e "==" (Lit l))]
  findPattern WilP        = return $ const [(Nothing, Nothing)]
  findPattern (VarP v t)  = do
    t <- fromType t
    return $ \e -> [(Just $ CDeclaration (v, t) e, Nothing)]
  findPattern (AppP n xs) = do
    xs' <- zipWithM (\x y -> (x,) <$> findPattern y) (["v" ++ show i | i <- [0..]]) xs
    return $ \e -> do
      let xs'' = map (\(x, f) -> f (CStructProp e x)) xs'
      concat ([(Nothing, Just $ CBinCall (CStructProp e "TYPE") "==" (CVariable $ n ++ "C"))] : xs'') 

  compileCase :: MonadCompiler m => TypedPattern -> CppAST -> [CppAST] -> m CppAST
  compileCase (VarP n t) = do
    \x b -> getEnv >>= \e -> case M.lookup n e of
      Just _ -> return $
        let cond = CBinCall (CStructProp x "TYPE") "==" (CVariable $ n ++ "C")
          in CIf cond $ CSequence b
      Nothing -> do
        t <- fromType t
        return $  CSequence [CDeclaration (n, t) x, CSequence b]
  compileCase (AppP n args) = do
    let args' x = concat <$> zipWithM (\arg v -> do
              f <- findPattern arg
              return $ f (CStructProp x v)) args (["v" ++ show i | i <- [0..]])
    \x b -> do
      args_ <- args' x
      let lets   = map (fromJust . fst) $ filter (isJust . fst) args_
      let cs = map (fromJust . snd) $ filter (isJust . snd) args_
      let cond  = CBinCall (CStructProp x "TYPE") "==" (CVariable $ n ++ "C")
          conds = case cs of
                    (c:cs) -> cond `and` foldl and c cs
                    _ -> cond
        in return $ CIf conds $ CSequence $ lets ++ b
  compileCase WilP = do
    \x b -> return $ CSequence b
  compileCase (LitP l) = do
    \x b -> return $
      let cond = CBinCall x "===" (Lit l)
        in CIf cond $ CSequence b