module TPL.Env (get, set, define, bindVars, getPrecedence, setPrecedence) where

import Control.Applicative
import Control.Monad.Error

import qualified Data.Map as M
import Data.Maybe
import Data.IORef

import TPL.Error
import TPL.Value

exists :: Env -> String -> IO Bool
exists env name = M.member name <$> readIORef env

und :: String -> IOThrowsError TPLValue
und = throwError . UndefinedVariable

get :: Env -> String -> IOThrowsError TPLValue
get env name = M.lookup name <$> liftIO (readIORef env) >>= maybe (und name) return 

set :: Env -> String -> TPLValue -> IOThrowsError TPLValue
set env name val = liftIO (M.lookup name <$> readIORef env) >>= maybe (und name)
                   (\ ref -> liftIO $ writeIORef ref val >> return val)

define :: Env -> String -> TPLValue -> IOThrowsError TPLValue
define env name val = do bound <- liftIO (exists env name)
                         if bound 
                           then set env name val >> return ()
                           else liftIO $ do value   <- newIORef val
                                            currEnv <- readIORef env
                                            writeIORef env $ M.insert name val currEnv
                         return val

bindVars :: Env -> [(String, TPLValue)] -> IO Env
bindVars env bindings = readIORef env >>= extended >>= newIORef
  where extended newEnv = (++ newEnv) <$> mapM addBinding bindings
        addBinding (name, val) = newIORef val >>= \ ref -> return (name, ref)

defaultPrecedence :: Num a => a
defaultPrecedence = 10

getPrecedence :: Env -> String -> IOThrowsError TPLValue
getPrecedence env op = let prec = "precedenceOf" ++ op in
  (liftIO $ lookup prec <$> readIORef env) >>= maybe (return $ Number defaultPrecedence) (const $ get env prec)

setPrecedence :: Env -> String -> Integer -> IOThrowsError TPLValue
setPrecedence env op precedence = define env ("precedenceOf" ++ op) $ Number precedence