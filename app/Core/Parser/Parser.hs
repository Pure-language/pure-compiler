{-# LANGUAGE BlockArguments #-}
module Core.Parser.Parser where
  import Text.Parsec
  import Text.Parsec.Expr
  import Text.Parsec.Char
  import Text.Parsec.String
  import Data.Functor
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Language (emptyDef)
  import Text.Parsec.Token (GenTokenParser)
  import Data.Functor.Identity (Identity)
  import Control.Applicative (Alternative(some))
  import Core.Parser.AST
  import Debug.Trace (traceShow)
  import Data.Maybe (fromMaybe, isJust)
  import Prelude hiding (break)

  {- LEXER PART -}
  languageDef =
    emptyDef {  Token.commentStart    = "/*"
              , Token.commentEnd      = "*/"
              , Token.commentLine     = "//"
              , Token.identStart      = letter <|> char '_'
              , Token.identLetter     = alphaNum <|> char '_'
              , Token.reservedNames   = ["let", "=", "fun", "if", "then", "else", "return", "extern", "match", "in", "for", "impl"," extension", "struct", "mut", "while"]
              , Token.reservedOpNames = ["(", ")", "*", "+", "-", "/", "{", "}", "[", "]", "->", "<", ">"] }

  lexer :: GenTokenParser String u Identity
  lexer = Token.makeTokenParser languageDef

  identifier :: Parser String
  identifier = Token.identifier lexer

  reserved :: String -> Parser ()
  reserved = Token.reserved lexer

  reservedOp :: String -> Parser ()
  reservedOp = Token.reservedOp lexer

  parens :: Parser a -> Parser a
  parens = Token.parens lexer

  integer :: Parser Integer
  integer = Token.integer lexer

  whiteSpace :: Parser ()
  whiteSpace = Token.whiteSpace lexer

  comma :: Parser String
  comma = Token.comma lexer

  commaSep :: Parser a -> Parser [a]
  commaSep = Token.commaSep lexer

  semi :: Parser String
  semi = Token.semi lexer

  {- PARSER PART -}

  type Pure a = Parser (Located a)

  parser :: Pure Statement
  parser = whiteSpace *> topLevel

  locate :: Parser a -> Pure a
  locate p = do
    start <- getPosition
    r <- p
    end <- getPosition
    return (r :> (start, end))

  -- Type parsing --

  buildFun :: [Declaration] -> Declaration -> Declaration
  buildFun xs t = go (reverse xs) t
    where go [] t = t
          go (x:xs) t = AppE (go xs t) x

  generics :: Parser [Declaration]
  generics = Token.angles lexer $ commaSep type'

  type' :: Parser Declaration
  type' = buildExpressionParser table typeTerm
    where table = [
            [ 
              Postfix $ makeUnaryOp (do
                args <- Token.angles lexer $ commaSep type'
                return (buildFun args)) 
            ]]

  typeTerm :: Parser Declaration
  typeTerm =  try (reserved "char" $> CharE)
       <|> struct <|> ref <|> tuple
       <|> try (reserved "str" $> StrE)
       <|> try (reserved "int" $> IntE)
       <|> try (reserved "float" $> FloatE)
       <|> try (reserved "void" $> VoidE)
       <|> try (reserved "bool" $> BoolE)
       <|> arrow <|> generic <|> array
       <|> parens type'

  tuple :: Parser Declaration
  tuple = do
    (a, b) <- Token.parens lexer $ do
      a <- type'
      comma
      b <- type'
      return (a, b)
    return (AppE (AppE (Id "Pair" []) a) b)

  struct :: Parser Declaration
  struct = do
    reserved "struct"
    reservedOp "{"
    fields <- commaSep (do
      name <- identifier
      reserved ":"
      t <- type'
      return (name, t))
    whiteSpace *> reservedOp "}"
    return $ StructE fields

  field :: Parser (String, Declaration)
  field = do
    name <- identifier
    reservedOp ":"
    type' <- type'
    return (name, type')

  generic :: Parser Declaration
  generic = do
    name <- identifier <|> (reserved "[]" $> "[]")
    args <- fromMaybe [] <$> optionMaybe (do
      reservedOp ":"
      sepBy1 identifier (reservedOp "+"))
    return $ Id name args

  array :: Parser Declaration
  array = AppE (Id "[]" []) <$> try (Token.brackets lexer type')

  arrow :: Parser Declaration
  arrow = do
    reserved "fun"
    annot <- fromMaybe [] <$> optionMaybe generics
    args <- parens $ commaSep type'
    Arrow annot args <$> type'

  ref :: Parser Declaration
  ref = do
    reserved "ref"
    Ref <$> type'

  -- Statement parsing --

  topLevel :: Pure Statement
  topLevel = choice [
      public,
      import',
      extern,
      enum,
      structureStmt,
      instance',
      class',
      functionStmt,
      assignment,
      mutable
    ]

  public :: Pure Statement
  public = do
    s <- getPosition
    reserved "pub" 
    e <- getPosition
    (:> (s, e)) . Public <$> topLevel

  statement :: Pure Statement
  statement = choice [
      extern,
      enum,
      structureStmt,
      continue,
      break,
      match <?> "pattern matching",
      modification,
      try stmtExpr,
      functionStmt,
      assignment,
      mutable,
      condition,
      return',
      for,
      while,
      block
    ]

  continue :: Pure Statement
  continue = do
    s <- getPosition
    reserved "continue"
    e <- getPosition
    return $ Continue :> (s, e)
  
  break :: Pure Statement
  break = do
    s <- getPosition
    reserved "break"
    e <- getPosition
    return $ Break :> (s, e)

  class' :: Pure Statement
  class' = do
    s <- getPosition
    reserved "extension"
    name <- identifier
    gen <- fromMaybe [] <$> optionMaybe generics
    reservedOp "{"
    fields <- commaSep field
    spaces *> reservedOp "}"
    e <- getPosition
    return $ Class gen name fields :> (s, e)

  generics' :: Parser [(String, [String])]
  generics' = do
    Token.angles lexer $ commaSep (do
      name <- identifier
      args <- fromMaybe [] <$> optionMaybe (do
        reservedOp ":"
        sepBy1 identifier (reservedOp "+"))
      return (name, args))

  instance' :: Pure Statement
  instance' = do
    s <- getPosition
    reserved "impl"
    gen <- fromMaybe [] <$> optionMaybe generics'
    name <- identifier
    reserved "for"
    t <- type'
    isDefault <- isJust <$> optionMaybe (reserved "override")
    reservedOp "{"
    fields <- commaSep $ do
      reserved "let"
      name <- identifier
      reservedOp "="
      expr <- expression
      return (name, expr)
    reservedOp "}"
    e <- getPosition
    return $ Instance gen name t fields isDefault :> (s, e)

  extern :: Pure Statement
  extern = do
    s <- getPosition
    reserved "extern"
    name <- identifier
    annot <- fromMaybe [] <$> optionMaybe generics
    reserved ":"
    ret <- type'
    e <- getPosition
    return $ Extern annot name ret :> (s, e)

  enum :: Pure Statement
  enum = do
    s <- getPosition
    reserved "enum"
    name <- identifier
    gen <- fromMaybe [] <$> optionMaybe generics
    reservedOp "{"
    values <- commaSep enumField
    reservedOp "}"
    e <- getPosition
    return $ Enum name gen values :> (s, e)

  enumField :: Parser (String, Maybe [Declaration])
  enumField = do
    name <- identifier
    ty <- optionMaybe $ parens (commaSep type')
    return (name, ty)

  return' :: Pure Statement
  return' = do
    s <- getPosition
    reserved "return"
    r <- expression
    e <- getPosition
    return (Return r :> (s, e))

  condition :: Pure Statement
  condition = do
    s <- getPosition
    reserved "if"
    cond <- expression <?> "condition"
    stmt <- statement <?> "then statement"
    reserved "else"
    stmt2 <- statement <?> "else statement"
    s2 <- getPosition
    return $ If cond stmt stmt2 :> (s, s2)

  stmtExpr :: Pure Statement
  stmtExpr = do
    e :> s <- expression
    return (Expression e :> s)

  modification :: Pure Statement
  modification = do
    s <- getPosition
    name <- try $ expression <* reservedOp "="
    e <- expression
    s2 <- getPosition
    return (Modified name e :> (s, s2))

  assignment :: Pure Statement
  assignment = do
    s <- getPosition
    reserved "let"
    (lhs :> _) <- annoted
    whiteSpace >> reserved "="
    rhs <- expression
    e <- getPosition
    return (Assignment lhs rhs :> (s, e))

  mutable :: Pure Statement
  mutable = do
    s <- getPosition
    reserved "mut"
    (lhs :> _) <- annoted
    whiteSpace >> reserved "="
    rhs@(_ :> p) <- expression
    e <- getPosition
    return (Assignment lhs (Reference rhs :> p) :> (s, e))

  for :: Pure Statement
  for = do
    s <- getPosition
    reserved "for"
    name <- identifier
    reserved "in"
    expr <- expression
    stmt <- statement
    e <- getPosition
    return $ For name expr stmt :> (s, e)

  while :: Pure Statement
  while = do
    s <- getPosition
    reserved "while"
    cond <- expression
    stmt <- statement
    e <- getPosition
    return $ While cond stmt :> (s, e)

  import' :: Pure Statement
  import' = do
    s <- getPosition
    reserved "import"
    m <- sepBy1 identifier (string "::")
    e <- getPosition
    return $ Import m:> (s, e)

  block :: Pure Statement
  block = do
    (_ :> s) <- locate $ reserved "{"
    stmts <- many (statement <* optionMaybe (reserved ";")) <?> "statement"
    reserved "}"
    return (Sequence stmts :> s)

  structureStmt :: Pure Statement
  structureStmt = do
    s <- getPosition
    reserved "struct"
    name <- identifier
    gen <- fromMaybe [] <$> optionMaybe generics
    reservedOp "{"
    fields <- commaSep (do
      f <- identifier
      reservedOp ":"
      t <- type'
      return (f, t))
    reservedOp "}"
    e <- getPosition
    return (Record name gen fields :> (s, e))

  functionStmt :: Pure Statement
  functionStmt = do
    s <- getPosition
    reserved "fun"
    name <- identifier
    gen <- fromMaybe [] <$> optionMaybe generics
    args <- parens $ commaSep annoted
    let args' = map (\(a :> _) -> a) args
    let argsTy = map (\(_ :@ ann) -> ann) args'
    ret <- optionMaybe type'
    body <- spaces *> block
    e <- getPosition
    return $ Assignment (name :@ (Arrow gen <$> sequence argsTy <*> ret)) (Lambda gen args' body :> (s, e)) :> (s, e)

  -- Expression parsing --

  expression :: Pure Expression
  expression = buildExpressionParser table term

  term :: Pure Expression
  term = try float <|> number <|> stringLit <|> charLit <|> list
      <|> (letIn <?> "let expression")
      <|> (function <?> "lambda")
      <|> (tupleish <?> "tuple")
      <|> (structure <?> "structure")
      <|> (variable <?> "variable")
      <|> (parens expression <?> "expression")

  tupleish :: Pure Expression
  tupleish = do
    s <- getPosition
    (a, b) <- parens $ do
      a <- expression
      reservedOp ","
      b <- expression
      return (a, b)
    e <- getPosition
    return $ Structure "Pair" [("fst", a), ("snd", b)] :> (s, e)

  letIn :: Pure Expression
  letIn = do
    s <- getPosition
    reserved "let"
    (lhs :> _) <- annoted
    reserved "="
    rhs <- expression
    reserved "in"
    body <- expression
    e <- getPosition
    return $ LetIn lhs rhs body :> (s, e)

  match :: Pure Statement
  match = do
    s <- getPosition
    reserved "match"
    expr <- expression
    cases <- Token.braces lexer $ commaSep case'
    e <- getPosition
    return $ Match expr cases :> (s, e)

  case' :: Parser (Located Expression, Located Statement)
  case' = do
    expr <- expression
    reserved "->"
    stmt <- statement
    return (expr, stmt)

  float :: Pure Expression
  float = do
    s <- getPosition
    num <- many1 digit
    char '.'
    dec <- many1 digit
    e <- getPosition
    return $ Literal (F ((read (num ++ "." ++ dec) :: Float) :> (s, e))) :> (s, e)

  list :: Pure Expression
  list = do
    (elems :> pos) <- locate $ Token.brackets lexer (commaSep expression)
    return (List elems :> pos)

  charLit :: Pure Expression
  charLit = do
    s <- getPosition
    (c :> pos) <- locate $ Token.charLiteral lexer
    e <- getPosition
    return (Literal (C (c :> pos)) :> (s, e))

  structure :: Pure Expression
  structure = do
    s <- getPosition
    reserved "struct"
    name <- identifier
    reservedOp "{"
    fields <- commaSep (do
      f <- identifier
      reservedOp ":"
      t <- expression
      return (f, t))
    reservedOp "}"
    e <- getPosition
    return (Structure name fields :> (s, e))

  stringLit :: Pure Expression
  stringLit = do
    s@(_ :> pos) <- locate $ Token.stringLiteral lexer
    return (Literal (S s) :> pos)

  number :: Pure Expression
  number = do
    (n :> s) <- locate integer
    return (Literal (I $ n :> s) :> s)

  variable :: Pure Expression
  variable = do
    (v :> s) <- locate identifier
    cast <- fromMaybe [] <$> optionMaybe (reservedOp "<" *> commaSep type' <* reservedOp ">")
    return (Variable v cast :> s)

  annoted :: Pure (Annoted String)
  annoted = do
    s <- getPosition
    v <- identifier
    ty <- optionMaybe $ reserved ":" *> type'
    s2 <- getPosition
    return ((v :@ ty) :> (s, s2))

  annoted' :: Parser a -> Pure (Annoted a)
  annoted' p = do
    s <- getPosition
    v <- p
    ty <- optionMaybe $ reserved ":" *> type'
    s2 <- getPosition
    return ((v :@ ty) :> (s, s2))

  function :: Pure Expression
  function = do
    s <- getPosition
    reserved "fun"
    annot <- fromMaybe [] <$> optionMaybe generics
    args <- parens $ commaSep annoted
    let args' = map (\(a :> _) -> a) args
    body <- statement <?> "function body"
    s2 <- getPosition
    return (Lambda annot args' body :> (s, s2))

  makeUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
  makeUnaryOp s = foldr1 (.) . reverse <$> some s

  loc :: Located a -> (SourcePos, SourcePos)
  loc (a :> s) = s
  
  table :: [[Operator String () Identity (Located Expression)]]
  table = [
      [Infix (do
        char '`'
        s <- getPosition
        fun <- identifier
        e' <- getPosition
        char '`'
        return (\x@(_ :> (p, _)) y@(_ :> (_, e)) -> FunctionCall (Variable fun [] :> (s, e')) [x, y] :> (p, e) )) AssocLeft],
      [Postfix $ makeUnaryOp postfix],
      [Prefix $ makeUnaryOp prefix],
      equalities,
      [Postfix $ do
        reserved "?"
        thn <- expression
        reserved ":"
        els <- expression
        return (\x@(_ :> (p, _)) -> Ternary x thn els :> (p, snd $ loc els))],
      [Infix (reservedOp "*" >> return (\x@(_ :> (s, _)) y@(_ :> (_, e)) -> BinaryOp "*" x y :> (s, e))) AssocLeft,
       Infix (reservedOp "/" >> return (\x@(_ :> (s, _)) y@(_ :> (_, e)) -> BinaryOp "/" x y :> (s, e))) AssocLeft],
      [Infix (reservedOp "+" >> return (\x@(_ :> (s, _)) y@(_ :> (_, e)) -> BinaryOp "+" x y :> (s, e))) AssocLeft,
       Infix (reservedOp "-" >> return (\x@(_ :> (s, _)) y@(_ :> (_, e)) -> BinaryOp "-" x y :> (s, e))) AssocLeft]
    ]
    where postfix = call <|> object <|> index
          call = do
            args <- parens $ commaSep expression
            e <- getPosition
            return $ \x@(_ :> (s, _)) -> FunctionCall x args :> (s, e)
          object = do
            reservedOp "."
            object <- identifier
            e <- getPosition
            return $ \x@(_ :> (_, s)) -> Object x object :> (e, s)
          index = do
            index' <- Token.brackets lexer expression
            e <- getPosition
            return $ \x@(_ :> (p, _)) -> Index x index' :> (p, e)

          -- Equality operators
          equalityOp = ["==", "!=", "<", ">", "<=", ">="]
          equalities = map (\op -> Infix (reservedOp op >> return (\x@(_ :> (s, _)) y@(_ :> (_, e)) -> BinaryOp op x y :> (s, e))) AssocLeft) equalityOp

          -- Prefix operators
          prefix = ref <|> unref
          ref = do
            s <- getPosition
            reserved "ref"
            return $ \x@(_ :> (_, e)) -> Reference x :> (s, e)
          unref = do
            s <- getPosition
            reservedOp "*"
            return $ \x@(_ :> (_, e)) -> Unreference x :> (s, e)
  parsePure :: String -> String -> Either ParseError [Located Statement]
  parsePure = runParser (many parser <* eof) ()