module TPL.Env where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import TPL.Error
import TPL.Value

und :: String -> Either Error a
und = Left . UndefinedVariable

getLocal :: Env -> String -> Result
getLocal env name = maybe (und name) Right $ M.lookup name env

setLocal :: Env -> String -> Value -> Either Error Env
setLocal env name val = maybe (und name) (const newEnv) $ M.lookup name env
  where newEnv = Right $ M.insert name val env