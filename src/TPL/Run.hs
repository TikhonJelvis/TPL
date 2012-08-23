module TPL.Run (repl, runFile) where
 
import System.Directory   (getCurrentDirectory)
import System.Environment (getEnv)
import System.IO

import TPL.Eval           (baseEnv, evalString)
import TPL.Value

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalAndPrint :: EnvRef -> String -> IO ()
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
          _    <- evalString env $ "TPL_PATH := '" ++ path ++ "'"
          _    <- evalString env $ "loadObj (get '*current*') '" ++ path ++ "/base.tpl'"
          until_ ((== "--quit") . dropWhile (/= '-')) (readPrompt "λ>") $ evalAndPrint env

runFile :: FilePath -> IO ()
runFile path = do code <- readFile path
                  baseEnv >>= (`evalString` code) >>= putStrLn