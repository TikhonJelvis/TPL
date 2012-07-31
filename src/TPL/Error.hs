module TPL.Error where

import Data.Functor                  ((<$>))
import Data.List                     (intercalate)

import Text.ParserCombinators.Parsec (ParseError)

import TPL.Value

type Result = Either Error Value

data Error = Parser ParseError
           | BadOp String
           | MissingOperand String
           | TypeMismatch String String
           | UndefinedVariable String
           | BadNativeCall String [Term]
           | Default String deriving (Show)

showError :: Error -> String
showError (Parser err) = show err
showError (BadOp op) = "Unknown operator " ++ op
showError (Default str) = str
showError (TypeMismatch expected got) = "Wrong type. Expected " ++ expected ++ "; got " ++ got ++ "."
showError (MissingOperand op) = "Missing operand for " ++ op
showError (UndefinedVariable var) = "Variable " ++ var ++ "is not defined."
showError (BadNativeCall name expr) = "Invalid native call to " ++ name ++ ": " ++ show expr ++ "." 

showErrorStack :: Error -> [Term] -> String
showErrorStack err stack = showError err ++ "\nStack Trace:\n" ++ intercalate "\n" (show <$> stack)
