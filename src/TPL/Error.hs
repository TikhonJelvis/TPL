module TPL.Error where

import qualified Control.Monad.Error as E

import Data.Functor                  ((<$>))
import Data.List                     (intercalate)

import Text.ParserCombinators.Parsec (ParseError)

import TPL.Value

type Result a = E.ErrorT Error IO a

data Error = Error [Term] ErrorType

instance E.Error Error where
  noMsg  = Error [] $ Default "Oh no, something went wrong!"
  strMsg = Error [] . Default

data ErrorType = Parser ParseError
               | BadOp String
               | MissingOperand String
               | TypeMismatch String Value
               | UndefinedVariable Term
               | BadNativeCall String [Term]
               | TooManyArguments Value
               | Default String deriving (Show)

showError :: ErrorType -> String
showError (Parser err) = show err
showError (BadOp op) = "Unknown operator " ++ op
showError (Default str) = str
showError (TypeMismatch expected got) = "Wrong type. Expected " ++ expected ++ "; got " ++ showType got ++ "."
showError (MissingOperand op) = "Missing operand for " ++ op
showError (UndefinedVariable var) = "Variable " ++ show var ++ "is not defined."
showError (BadNativeCall name expr) = "Invalid native call to " ++ name ++ ": " ++ show expr ++ "." 
showError (TooManyArguments fn) = "Too many arguments passed to " ++ show fn

showErrorStack :: Error -> String
showErrorStack (Error stack err) = showError err ++ "\nStack Trace:\n" ++ intercalate "\n" (show <$> stack)

pushTrace :: Error -> Term -> Error
pushTrace (Error stack err) term = Error (term:stack) err

liftEither :: Either Error a -> Result a
liftEither (Left err)  = E.throwError err
liftEither (Right val) = return val

throw :: ErrorType -> Result a
throw = E.throwError . Error []