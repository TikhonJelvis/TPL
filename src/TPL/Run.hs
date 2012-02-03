module TPL.Run (repl, runFile) where
 
import Control.Applicative
import Control.Monad.Error

import System.Environment
import System.IO

import TPL.Coerce
import TPL.Error
import TPL.Env
import TPL.Eval
import TPL.Value

unpack :: ThrowsError TPLValue -> TPLValue
unpack (Right val) = val
unpack (Left err) = String $ show err

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env exp = evalString env exp >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = 
  do result <- prompt
     if pred result 
       then return ()
       else action result >> until_ pred prompt action

repl :: IO ()
repl = baseEnv >>= until_ (== "quit") (readPrompt "~>") . evalAndPrint

runFile :: FilePath -> IO ()
runFile path = do code <- readFile path
                  baseEnv >>= (`evalString` code) >>= putStrLn