module TPL.Error where

import Data.Functor                  ((<$>))
import Data.List                     (intercalate)

import Text.ParserCombinators.Parsec (ParseError)

import TPL.Value

type Result = Either Error Value

data Error = Error [Term] ErrorType

data ErrorType = Parser ParseError
               | BadOp String
               | MissingOperand String
               | TypeMismatch String String
               | UndefinedVariable String
               | BadNativeCall String [Term]
               | Default String deriving (Show)

showError :: ErrorType -> String
showError (Parser err) = show err
showError (BadOp op) = "Unknown operator " ++ op
showError (Default str) = str
showError (TypeMismatch expected got) = "Wrong type. Expected " ++ expected ++ "; got " ++ got ++ "."
showError (MissingOperand op) = "Missing operand for " ++ op
showError (UndefinedVariable var) = "Variable " ++ var ++ "is not defined."
showError (BadNativeCall name expr) = "Invalid native call to " ++ name ++ ": " ++ show expr ++ "." 

showErrorStack :: Error -> String
showErrorStack (Error stack err) = showError err ++ "\nStack Trace:\n" ++ intercalate "\n" (show <$> stack)

pushTrace :: Error -> Term -> Error
pushTrace (Error stack err) term = Error (term:stack) err