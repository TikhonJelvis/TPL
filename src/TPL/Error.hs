module TPL.Error where

import Control.Monad.Error           (Error, noMsg, strMsg)

import Data.Functor                  ((<$>))
import Data.List                     (intercalate)

import Text.ParserCombinators.Parsec (ParseError)

import TPL.Value

data TPLError = Parser ParseError
              | BadOp String
              | MissingOperand String
              | TypeMismatch String String
              | UndefinedVariable String
              | BadNativeCall String [Term]
              | Default String deriving (Show)
                                        
instance Error TPLError where
  noMsg  = Default "An error has occured!"
  strMsg = Default

showTPLError :: TPLError -> String
showTPLError (Parser err) = show err
showTPLError (BadOp op) = "Unknown operator " ++ op
showTPLError (Default str) = str
showTPLError (TypeMismatch expected got) = "Wrong type. Expected " ++ expected ++ "; got " ++ got ++ "."
showTPLError (MissingOperand op) = "Missing operand for " ++ op
showTPLError (UndefinedVariable var) = "Variable " ++ var ++ "is not defined."
showTPLError (BadNativeCall name expr) = "Invalid native call to " ++ name ++ ": " ++ show expr ++ "." 

showErrorStack :: TPLError -> [Term] -> String
showErrorStack err stack = showTPLError err ++ "\nStack Trace:\n" ++ intercalate "\n" (show <$> stack)
