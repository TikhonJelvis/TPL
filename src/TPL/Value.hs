module TPL.Value where

import Data.IORef
import qualified Data.Map as M

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
           | Function EnvRef [Term] Term
           | Object EnvRef deriving (Show, Eq)
             
-- Potentially make this faster in the future.
type Env = M.Map String Value
                                    
newtype EnvRef = EnvRef (IORef Env) deriving (Eq)

instance Show EnvRef where show _ = "<env>"