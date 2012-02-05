module TPL.Run (repl, runFile) where
 
import System.IO

import TPL.Eval
import TPL.Value

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = 
  do result <- prompt
     if predicate result 
       then return ()
       else action result >> until_ predicate prompt action

repl :: IO ()
repl = baseEnv >>= until_ (== "quit") (readPrompt "~>") . evalAndPrint

runFile :: FilePath -> IO ()
runFile path = do code <- readFile path
                  baseEnv >>= (`evalString` code) >>= putStrLn