module TPL.Value where

type Number = Integer

data Term = NullLiteral
          | Id String
          | NumericLiteral Number
          | StringLiteral String
          | BoolLiteral Bool
          | Operator String
          | ListLiteral [Term]
          | Lambda [Term] Term
          | Expression [Term]
          | Block [Term]
          | ObjectLiteral [(Term, Term)] deriving (Show, Eq)

data Value = Null
           | Var String
           | Number Number
           | String String
           | Bool Bool
           | List [Value]
           | Function Env [Term] Term
           | Object Env deriving (Show, Eq)
             
type Env = [(String, Value)]