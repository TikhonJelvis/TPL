module TPL.Env where

import Control.Applicative
import Control.Monad.Error

import Data.Maybe
import Data.IORef

import TPL.Error
import TPL.Value

exists :: Env -> String -> IO Bool
exists env name = isJust . lookup name <$> readIORef env

getRef env name = liftIO $ lookup name <$> readIORef env

und :: String -> IOThrowsError TPLValue
und = throwError . UndefinedVariable

get :: Env -> String -> IOThrowsError TPLValue
get env name = liftIO (lookup name <$> readIORef env) >>= maybe (und name) (liftIO . readIORef)
       
set :: Env -> String -> TPLValue -> IOThrowsError TPLValue
set env name val = liftIO (lookup name <$> readIORef env) >>= maybe (und name)
                   (\ ref -> liftIO $ writeIORef ref val >> return val)

define :: Env -> String -> TPLValue -> IOThrowsError TPLValue
define env name val = do exists <- liftIO (exists env name)
                         if exists 
                           then set env name val >> return ()
                           else liftIO $ do value   <- newIORef val
                                            currEnv <- readIORef env
                                            writeIORef env $ (name, value) : currEnv
                         return val

bindVars :: Env -> [(String, TPLValue)] -> IO Env
bindVars env bindings = readIORef env >>= extend bindings >>= newIORef
  where extend bindings env = (++ env) <$> mapM addBinding bindings
        addBinding (name, val) = newIORef val >>= \ ref -> return (name, ref)

defaultPrecedence = 10

getPrecedence :: Env -> String -> IOThrowsError TPLValue
getPrecedence env op = let prec = "precedenceOf" ++ op in
  (liftIO $ lookup prec <$> readIORef env) >>= maybe (return $ Number defaultPrecedence) (const $ get env prec)

setPrecedence :: Env -> String -> Integer -> IOThrowsError TPLValue
setPrecedence env op precedence = define env ("precedenceOf" ++ op) $ Number precedence