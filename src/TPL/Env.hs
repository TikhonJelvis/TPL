module TPL.Env where

import Data.Functor ((<$>), (<$))
import Data.IORef   (readIORef, writeIORef, modifyIORef)
import qualified Data.Map as M
import Data.Maybe   (fromMaybe)

import TPL.Error
import TPL.Value

und :: String -> Either Error a
und = Left . UndefinedVariable

getEnv :: String -> Env -> Result
getEnv name env = maybe (und name) Right $ M.lookup name env

setEnv :: String -> Value -> Env -> Either Error Env
setEnv name val env = maybe (und name) (const newEnv) $ M.lookup name env
  where newEnv = Right $ M.insert name val env
        
defineEnv :: String -> Value -> Env -> Env 
defineEnv name val env = M.insert name val env
        
getEnvRef :: String -> EnvRef -> IO Result
getEnvRef name (EnvRef ref) = getEnv name <$> readIORef ref

setEnvRef :: String -> Value -> EnvRef -> IO Result
setEnvRef name val (EnvRef ref) = do env <- readIORef ref
                                     case setEnv name val env of
                                       Left err -> return $ Left err
                                       Right env' -> Right val <$ writeIORef ref env'
                                       
defineEnvRef :: String -> Value -> EnvRef  -> IO Value
defineEnvRef name val (EnvRef ref) = val <$ (modifyIORef ref $ defineEnv name val)