module TPL.Env where

import Control.Monad.Error (throwError, liftIO)

import Data.Functor        ((<$>), (<$))
import Data.IORef          (readIORef, writeIORef, modifyIORef, newIORef)
import qualified Data.Map as M

import TPL.Error
import TPL.Native
import TPL.Value

und :: Value -> Error
und = Error [] . UndefinedVariable

getEnv :: Value -> Env -> Either Error Value
getEnv name env = maybe (Left $ und name) Right $ M.lookup name env

setEnv :: Value -> Value -> Env -> Either Error Env
setEnv name val env = maybe (Left $ und name) (const newEnv) $ M.lookup name env
  where newEnv = Right $ M.insert name val env
        
defineEnv :: Value -> Value -> Env -> Env
defineEnv name val env = M.insert name val env
        
bindEnv :: [(Value, Value)] -> Env -> Env
bindEnv bindings env = M.union (M.fromList bindings) env

getEnvRef :: EnvRef -> Value -> Result Value
getEnvRef env (String "*current*") = return $ Object env
getEnvRef (EnvRef ref) name        = do env <- liftIO $ readIORef ref
                                        liftEither $ getEnv name env

setEnvRef :: EnvRef -> Value -> Value -> Result Value
setEnvRef (EnvRef ref) name val = do env <- liftIO $ readIORef ref
                                     case setEnv name val env of
                                       Left err   -> throwError err
                                       Right env' -> val <$ liftIO (writeIORef ref env')
                                       
defineEnvRef :: EnvRef -> Value -> Value -> Result Value
defineEnvRef (EnvRef ref) name val = liftIO $ val <$ (modifyIORef ref $ defineEnv name val)

bindEnvRef :: [(Value, Value)] -> EnvRef -> IO EnvRef
bindEnvRef bindings (EnvRef ref) = bindEnv bindings <$> readIORef ref >>= (EnvRef <$>) . newIORef

getPrecs :: IO [(String, Int)]
getPrecs = return []            -- TODO: Make this work!
