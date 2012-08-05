module TPL.Value where

import Data.Functor            ((<$>))
import Data.IORef
import Data.List               (intercalate)
import qualified Data.Map as M

type Number = Integer

data Code = Code {term :: Term, code :: String} deriving (Show, Eq, Ord)

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
          | ObjectLiteral [(Term, Term)] deriving (Show, Eq, Ord)

data Value = Null
           | Number Number
           | String String
           | Symbol String
           | Bool Bool
           | List [Value]
           | Function EnvRef [Term] Term
           | Object EnvRef deriving (Show, Eq, Ord)
                                    
display :: Term -> String
display NullLiteral         = "null"
display (Id name)           = name
display (NumericLiteral n)  = show n
display (StringLiteral str) = show str
display (BoolLiteral bool)  = if bool then "true" else "false"
display (Operator op)       = op
display (ListLiteral ls)    = "[" ++ displayList ", " ls ++ "]"
display (Lambda args body)  = "λ " ++ displayList " " args ++ " → " ++ display body
display (Expression terms)  = "(" ++ displayList " " terms ++ ")"
display (Block terms)       = displayList " " terms
display (ObjectLiteral _)   = "{...}" -- TODO: display object literals properly!

displayList :: String -> [Term] -> String
displayList sep = intercalate sep . map display
             
type Env = M.Map Value Value
                                    
newtype EnvRef = EnvRef (IORef Env) deriving (Eq)

instance Show EnvRef where show _ = "<env>"
instance Ord EnvRef where compare _ _ = EQ
                          
nullEnv :: IO EnvRef
nullEnv = EnvRef <$> newIORef M.empty
                           
showType :: Value -> String
showType Null       = "null"
showType Number{}   = "number"
showType String{}   = "string"
showType Bool{}     = "bool"
showType List{}     = "list"
showType Function{} = "function"
showType Object{}   = "object"