{-# LANGUAGE TupleSections #-}
module Core.AsyncChecker where
  import Core.Parser.AST
  import Core.TypeChecking.Type (unlocate)
  import Text.Parsec.Pos
  
  checkStmt :: Bool -> Located Statement -> Error (Located Statement)
  checkStmt b (s :> t) = case s of
    Expression e -> do
      e' <- checkExpression b (e :> t)
      return (Expression (unlocate e') :> t)
    Assignment a e -> (:> t) <$> (Assignment a <$> checkExpression b e)
    Modified e1 e2 -> (:> t) <$> (Modified <$> checkExpression b e1 <*> checkExpression b e2)
    If e1 e2 e3 -> (:> t) <$> (If <$> checkExpression b e1 <*> checkExpression b e2 <*> checkExpression b e3)
    Return e -> (:> t) <$> (Return <$> checkExpression b e)
    For i e1 e2 -> (:> t) <$> (For i <$> checkExpression b e1 <*> checkExpression b e2)
    While e1 e2 -> (:> t) <$> (While <$> checkExpression b e1 <*> checkExpression b e2)
    _ -> return $ s :> t
  
  checkToplevel :: Located Statement -> Error (Located Statement)
  checkToplevel (s :> t) = case s of
    Assignment a (Await e :> p) -> do
      e' <- checkExpression False e
      return (Assignment a (Await e' :> p) :> t)
    Assignment a e -> do
      e' <- checkExpression False e
      return (Assignment a e' :> t)
    Instance sub n ty f over -> (:> t) <$> (Instance sub n ty <$> mapM (\(x, t) -> (x,) <$> checkExpression False t) f <*> pure over) 
    Public s -> (:> t) <$> (Public <$> checkToplevel s)
    _ -> return $ s :> t

  type Error a = Either (String, Maybe String, (SourcePos, SourcePos)) a

  checkExpression :: Bool -> Located Expression -> Error (Located Expression)
  checkExpression t (FunctionCall f args :> p) = do
    f' <- checkExpression t f
    args' <- mapM (checkExpression t) args
    return (FunctionCall f' args' :> p)
  checkExpression t (Throw e :> p) = do
    e' <- checkExpression t e
    return (Throw e' :> p)
  checkExpression t (Lambda args ret e :> p) = do
    e' <- checkExpression t e
    return (Lambda args ret e' :> p)
  checkExpression t (Sequence s :> p) = do
    s' <- mapM (checkStmt t) s
    return (Sequence s' :> p)
  checkExpression t (Variable v args :> p) = return (Variable v args :> p)
  checkExpression t (Literal l :> p) = return (Literal l :> p)
  checkExpression t (BinaryOp op e1 e2 :> p) = do
    e1' <- checkExpression t e1
    e2' <- checkExpression t e2
    return (BinaryOp op e1' e2' :> p)
  checkExpression t (UnaryOp op e :> p) = do
    e' <- checkExpression t e
    return (UnaryOp op e' :> p)
  checkExpression t (Await e :> p) = do
    if t 
      then do
        e' <- checkExpression t e
        return (Await e' :> p)
      else Left ("Await is only allowed in toplevel statements and in asynchronous expressions", Just "Check for async keyword on the nearest block", p)
  checkExpression t (List xs :> p) = do
    xs' <- mapM (checkExpression False) xs
    return (List xs' :> p)
  checkExpression t (Index e1 e2 :> p) = do
    e1' <- checkExpression t e1
    e2' <- checkExpression t e2
    return (Index e1' e2' :> p)
  checkExpression t (Match e cs :> p) = do
    e' <- checkExpression t e
    cs' <- mapM (\(x, y) -> (,) x <$> checkExpression t y) cs
    return (Match e' cs' :> p)
  checkExpression t (LetIn s e b :> p) = do
    e' <- checkExpression t e
    b' <- checkExpression t b
    return (LetIn s e' b' :> p)
  checkExpression t (Structure n fs :> p) = (:> p) <$> (Structure n <$> mapM (\(x, y) -> (x,) <$> checkExpression t y) fs)
  checkExpression t (Object o f :> p) = (:> p) <$> (Object <$> checkExpression t o <*> pure f)
  checkExpression t (Ternary e1 e2 e3 :> p) = do
    e1' <- checkExpression t e1
    e2' <- checkExpression t e2
    e3' <- checkExpression t e3
    return (Ternary e1' e2' e3' :> p)
  checkExpression t (Reference e :> p) = do
    e' <- checkExpression t e
    return (Reference e' :> p)
  checkExpression t (Unreference e :> p) = do
    e' <- checkExpression t e
    return (Unreference e' :> p)
  checkExpression t (Cast e ty :> p) = do
    e' <- checkExpression t e
    return (Cast e' ty :> p)
  checkExpression t (Async e :> p) = do
    e' <- checkExpression True e
    return (Async e' :> p)