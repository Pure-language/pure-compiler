{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Modules.Pattern where
  import Core.TypeChecking.Type.AST (TypedPattern(..), Literal(..))
  import Core.Compiler.CodeGen.IR (IR(..), varify)
  import Core.Compiler.Type (MonadCompiler, getConstructor)
  import qualified Data.Map as M
  import Prelude hiding (and)
  import Data.Maybe (fromJust, isJust)
  import Control.Monad (zipWithM)

  and :: IR -> IR -> IR
  and l = IRBinCall l "&&"

  findPattern :: MonadCompiler m => TypedPattern -> m (IR -> [(Maybe IR, Maybe IR)])
  findPattern (LitP l)    =
    return $ \e -> [(Nothing, Just $ IRBinCall e "==" (IRLit l))]
  findPattern WilP        =
    return $ const [(Nothing, Nothing)]
  findPattern (VarP v t)  = do
    return $ \e -> [(Just $ IRDeclaration (varify v) e, Nothing)]
  findPattern (AppP n xs) = do
    xs' <- zipWithM (\x y -> (x,) <$> findPattern y) (["v" ++ show i | i <- [0..]]) xs
    return $ \e -> do
      let xs'' = map (\(x, f) -> f (IRStructProp e x)) xs'
      concat ([(Nothing, Just $ IRBinCall (IRStructProp e "type") "===" (IRLit (S n)))] : xs'')
  findPattern (StructP n args) = do
    args' <- mapM (\(x, y) -> (x,) <$> findPattern y) args
    return $ \e -> do
      let args'' = concatMap (\(x, f) -> f (IRStructProp e x)) args'
      concatMap (\(x, y) -> [(Nothing, Just $ IRIn (IRLit (S x)) e)]) args ++ args''

  compileCase :: MonadCompiler m => TypedPattern -> IR -> [IR] -> m IR
  compileCase (VarP n t) = do
    \x b -> getConstructor n >>= \case
      Just _ -> return $
        let cond = IRBinCall (IRStructProp x "type") "===" (IRLit $ S n)
          in IRIf cond $ IRSequence b
      Nothing -> return $ IRSequence [IRDeclaration (varify n) x, IRSequence b]
  compileCase (AppP n args) = do
    let args' x = concat <$> zipWithM (\arg v -> do
              f <- findPattern arg
              return $ f (IRStructProp x v)) args (["v" ++ show i | i <- [0..]])
    \x b -> do
      args_ <- args' x
      let lets   = map (fromJust . fst) $ filter (isJust . fst) args_
      let cs = map (fromJust . snd) $ filter (isJust . snd) args_
      let cond  = IRBinCall (IRStructProp x "type") "===" (IRLit $ S n)
          conds = case cs of
                    (c:cs) -> cond `and` foldl and c cs
                    _ -> cond
        in return $ IRIf conds $ IRSequence $ lets ++ b
  compileCase WilP = do
    \x b -> return $ IRSequence b
  compileCase (LitP l) = do
    \x b -> return $
      let cond = IRBinCall x "===" (IRLit l)
        in IRIf cond $ IRSequence b
  compileCase (StructP n args) = do
    let args' x = concat <$> zipWithM (\arg v -> do
              f <- findPattern arg
              return $ f (IRStructProp x v)) (map snd args) (map fst args)
    \x b -> do
      args_ <- args' x
      let lets   = map (fromJust . fst) $ filter (isJust . fst) args_
      let cs = map (fromJust . snd) $ filter (isJust . snd) args_
      let cond  = map (\(x', y) -> IRIn (IRLit (S x')) x) args
          conds = case cs of
                    c@(_:_) -> createAnd (c ++ cond)
                    _ -> createAnd cond
        in return $ IRIf conds $ IRSequence $ lets ++ b

  createAnd :: [IR] -> IR
  createAnd [] = error "test"
  createAnd [x] = x
  createAnd (x:xs) = x `and` createAnd xs