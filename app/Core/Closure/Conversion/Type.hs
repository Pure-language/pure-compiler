module Core.Closure.Conversion.Type where
  import Control.Monad.RWS (MonadState, modify, gets)
  import Control.Arrow (Arrow(second, first))
  import Data.Map (Map)
  
  type Environment = [String]
  type MonadClosure m = MonadState Closure m

  data Closure = Closure {
    environment :: Environment,
    closures :: [Statement],
    index :: Int,
    matches :: Map String String
  } deriving Show

  fresh :: MonadClosure m => m String
  fresh = do
    i <- gets index
    modify $ \s -> s { index = i + 1 }
    return $ "lambda" ++ show i

  addClosure :: MonadClosure m => Statement -> m ()
  addClosure c = modify $ \s -> s { closures = c : closures s }

  addEnv :: MonadClosure m => String -> m ()
  addEnv c = modify $ \s -> s { environment = c : environment s }

  unionEnv :: MonadClosure m => Environment -> m ()
  unionEnv es = modify $ \s -> s { environment = es ++ environment s }

  setEnv :: MonadClosure m => Environment -> m ()
  setEnv e = modify $ \s -> s { environment = e }

  data Expression
    = FunctionCall Expression [Expression]
    | Lambda [String] Statement
    | Variable String
    | Literal Literal
    | BinaryOp String Expression Expression
    | UnaryOp String Expression
    | List [Expression]
    | Index Expression Expression
    deriving (Show, Eq)

  data Statement
    = Assignment String Expression
    | If Expression Statement Statement
    | Sequence [Statement]
    | Expression Expression
    | Return Expression
    deriving (Show, Eq)

  data Literal
    = I Integer
    | F Float
    | S String
    | C Char
    deriving (Show, Eq)