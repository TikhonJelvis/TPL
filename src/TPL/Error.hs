module TPL.Error (TPLError (..), ThrowsError, IOThrowsError, 
                  liftThrows, runIOThrows) where

import Control.Applicative
import Control.Monad.Error

import Text.ParserCombinators.Parsec

import TPL.Value

data TPLError = Parser ParseError
              | BadOp String
              | MissingOperand String
              | TypeMismatch String String
              | UndefinedVariable String
              | BadNativeCall String [TPLValue]
              | Default String

showTPLE :: TPLError -> String
showTPLE (Parser err)                = show err
showTPLE (BadOp op)                  = "Unknown operator " ++ op
showTPLE (Default str)               = str
showTPLE (TypeMismatch expected got) = "Wrong type. Expected " ++ expected ++ "; got " ++ got ++ "."
showTPLE (MissingOperand op)         = "Missing operand for " ++ op
showTPLE (UndefinedVariable var)     = "Variable " ++ var ++ " is undefined"
showTPLE (BadNativeCall name expr)   = "Invalid native call to " ++ name ++ " " ++ show expr

instance Show TPLError where
  show err = "Error: " ++ showTPLE err ++ "."
instance Error TPLError where
  noMsg = Default "An error has occured!"
  strMsg = Default

type ThrowsError = Either TPLError
type IOThrowsError = ErrorT TPLError IO

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "Cannot extract from an error!"

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runErrorT (trapError action)
