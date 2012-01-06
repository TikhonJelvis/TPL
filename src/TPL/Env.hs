module TPL.Env where

import Control.Applicative
import Control.Monad.Error

import Data.Maybe
import Data.IORef

import TPL.Error
import TPL.Value

type Env = IORef [(String, IORef TPLValue)]

nullEnv = newIORef []

exists :: Env -> String -> IO Bool
exists env name = isJust . lookup name <$> readIORef env

get :: Env -> String -> IOThrowsError TPLValue
get env name = do env <- liftIO $ readIORef env
                  case lookup name env of
                    Just ref -> liftIO $ readIORef ref
                    Nothing  -> throwError $ UndefinedVariable name
       
set :: Env -> [TPLValue] -> IOThrowsError TPLValue
set env [Id name, val] = do env <- liftIO $ readIORef env
                            case lookup name env of
                              Just ref -> liftIO $ writeIORef ref val >> return val
                              Nothing  -> throwError $ UndefinedVariable name

define :: Env -> String -> TPLValue -> IOThrowsError TPLValue
define env name val = do exists <- liftIO (exists env name)
                         if exists 
                           then set env [(Id name), val] >> return ()
                           else liftIO $ do value   <- newIORef val
                                            currEnv <- readIORef env
                                            writeIORef env $ (name, value) : currEnv
                         return val

bindVars :: Env -> [(String, TPLValue)] -> IO Env
bindVars env bindings = readIORef env >>= extend bindings >>= newIORef
  where extend bindings env = (++ env) <$> mapM addBinding bindings
        addBinding (name, val) = newIORef val >>= \ ref -> return (name, ref)
