module TPL.Env where

import Control.Monad.Error (throwError, liftIO)

import Data.Functor        ((<$>), (<$))
import Data.IORef          (readIORef, writeIORef, modifyIORef)
import qualified Data.Map as M
import Data.Maybe          (fromMaybe)

import TPL.Error
import TPL.Value

und :: String -> Error
und = Error [] . UndefinedVariable

getEnv :: String -> Env -> Either Error Value
getEnv name env = maybe (Left $ und name) Right $ M.lookup name env

setEnv :: String -> Value -> Env -> Either Error Env
setEnv name val env = maybe (Left $ und name) (const newEnv) $ M.lookup name env
  where newEnv = Right $ M.insert name val env
        
defineEnv :: String -> Value -> Env -> Env 
defineEnv name val env = M.insert name val env
        
getEnvRef :: String -> EnvRef -> Result Value
getEnvRef name (EnvRef ref) = do env <- liftIO $ readIORef ref
                                 liftEither $ getEnv name env

setEnvRef :: String -> Value -> EnvRef -> Result Value
setEnvRef name val (EnvRef ref) = do env <- liftIO $ readIORef ref
                                     case setEnv name val env of
                                       Left err -> throwError err
                                       Right env' -> val <$ liftIO (writeIORef ref env')
                                       
defineEnvRef :: String -> Value -> EnvRef  -> IO Value
defineEnvRef name val (EnvRef ref) = val <$ (modifyIORef ref $ defineEnv name val)

getPrecs :: IO [(Term, Int)]
getPrecs = return []            -- TODO: Make this work!