module TPL.Env where

import Control.Applicative
import Control.Monad.Error

import Data.Maybe
import Data.IORef

import TPL.Error
import TPL.Value

exists :: Env -> String -> IO Bool
exists env name = isJust . lookup name <$> readIORef env

get :: Env -> String -> IOThrowsError TPLValue
get env name = do env <- liftIO $ readIORef env
                  case lookup name env of 
                    Just ref -> liftIO $ readIORef ref
                    Nothing  -> throwError $ UndefinedVariable name
       
set :: Env -> String -> TPLValue -> IOThrowsError TPLValue
set env name val = do env <- liftIO $ readIORef env
                      case lookup name env of
                        Just ref -> liftIO $ writeIORef ref val >> return val
                        Nothing  -> throwError $ UndefinedVariable name

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
getPrecedence env op = do let prec = "precedenceOf" ++ op
                          precedenceSet <- liftIO $ exists env prec
                          if precedenceSet
                            then get env $ "precedenceOf" ++ op
                            else return $ Number defaultPrecedence

setPrecedence :: Env -> String -> Integer -> IOThrowsError TPLValue
setPrecedence env op precedence = define env ("precedenceOf" ++ op) $ Number precedence