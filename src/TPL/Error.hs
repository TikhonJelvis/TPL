module TPL.Error where

import qualified Control.Monad.Error as E

import Data.Functor                  ((<$>))
import Data.List                     (intercalate)

import TPL.Value

showError :: ErrorType -> String
showError (Parser err) = show err
showError (BadOp op) = "Unknown operator " ++ op
showError (Default str) = str
showError (TypeMismatch expected got) = "Wrong type. Expected " ++ expected ++ "; got " ++ showType got ++ "."
showError (MissingOperand op) = "Missing operand for " ++ op
showError (UndefinedVariable var) = "Variable " ++ displayVal var ++ " is not defined."
showError (BadNativeCall name expr) = "Invalid native call to " ++ name ++ ": " ++ display (Expression expr) ++ "." 
showError (TooManyArguments fn) = "Too many arguments passed to " ++ displayVal fn

showErrorStack :: Error -> String
showErrorStack (Error stack err) = "Error: " ++ showError err ++ "\nStack Trace:\n" ++ intercalate "\n" (display <$> stack)

pushTrace :: Error -> Term -> Error
pushTrace (Error stack err) term = Error (term:stack) err

liftEither :: Either Error a -> Result a
liftEither (Left err)  = E.throwError err
liftEither (Right val) = return val

throw :: ErrorType -> Result a
throw = E.throwError . Error []