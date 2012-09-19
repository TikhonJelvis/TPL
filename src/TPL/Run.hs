module TPL.Run (repl, runFile) where

import           Control.Monad.Error (runErrorT)

import           System.Directory    (getCurrentDirectory)
import           System.Environment  (getEnv)
import           System.IO

import           TPL.Eval            (defineIn, evalString)
import           TPL.Native          (baseEnv)
import           TPL.Value

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalAndPrint :: EnvRef -> String -> IO ()
evalAndPrint env expr = evalString "<repl>" env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action =
  do result <- prompt
     if predicate result
       then return ()
       else action result >> until_ predicate prompt action

prelude :: IO EnvRef
prelude = do env <- baseEnv
             path <- catch (getEnv "TPL_PATH") (\ _ -> getCurrentDirectory)
             _    <- runErrorT $ defineIn env (String "TPL_PATH") $ String path
             _    <- evalString "<prelude>" env $ "loadObj (get '*current*') '" ++ path ++ "/base.tpl'"
             return $ env

repl :: IO ()
repl = do env  <- prelude
          until_ (== "--quit") (readPrompt "λ>") $ evalAndPrint env

runFile :: FilePath -> IO ()
runFile path = do code  <- readFile path
                  start <- prelude
                  evalString path start code >>= putStrLn
