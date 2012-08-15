module TPL.Value where

import qualified Control.Monad.Error as E

import Data.Functor            ((<$>))
import Data.IORef
import Data.List               (intercalate)
import qualified Data.Map as M

import Text.ParserCombinators.Parsec (ParseError)

type Number = Integer

data NativeOpr = NativeOpr (Term -> Result Value)

instance Eq NativeOpr where _ == _ = False
  
instance Show NativeOpr where show = const "<native code>"
instance Ord NativeOpr where compare _ _ = EQ

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

data Value = Null
           | Number Number
           | String String
           | Symbol String
           | Bool Bool
           | List [Value]
           | Function EnvRef [Term] Term
           | Object EnvRef
           | Native NativeOpr deriving (Show, Eq, Ord)

displayVal :: Value -> String
displayVal Null                   = "null"
displayVal (Number n)             = show n
displayVal (String s)             = show s
displayVal (Symbol s)             = s
displayVal (Bool b)               = if b then "true" else "false"
displayVal (List vs)              = "[" ++ intercalate ", " (displayVal <$> vs) ++ "]"
displayVal (Function _ args body) = display $ Lambda args body
displayVal (Object _)             = "{...}"
displayVal (Native opr)          = "<Native: " ++ show opr ++ ">"

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
showType Native{}   = "native"
showType Symbol{}   = "symbol"

                      -- Error-handling:
type Result a = E.ErrorT Error IO a

data Error = Error [Term] ErrorType deriving (Show)

instance E.Error Error where
  noMsg  = Error [] $ Default "Oh no, something went wrong!"
  strMsg = Error [] . Default

data ErrorType = Parser ParseError
               | BadOp String
               | MissingOperand String
               | TypeMismatch String Value
               | UndefinedVariable Value
               | BadNativeCall String [Term]
               | TooManyArguments Value
               | Default String deriving (Show)
