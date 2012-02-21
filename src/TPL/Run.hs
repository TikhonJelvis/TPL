module TPL.Run (repl, runFile) where
 
import System.Directory
import System.Environment
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
repl = do env  <- baseEnv
          path <- catch (getEnv "TPL_PATH") (\ _ -> getCurrentDirectory)
          evalString env $ "TPL_PATH := '" ++ path ++ "'"
          evalString env "load 'base'"
          until_ (== "quit") (readPrompt "λ>") $ evalAndPrint env

runFile :: FilePath -> IO ()
runFile path = do code <- readFile path
                  baseEnv >>= (`evalString` code) >>= putStrLn