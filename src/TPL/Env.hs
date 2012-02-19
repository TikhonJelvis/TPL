module TPL.Env (get, set, define, bindVars, getPrecedence, setPrecedence) where

import Control.Applicative
import Control.Monad.Error

import qualified Data.Map as M
import Data.IORef

import TPL.Error
import TPL.Value

exists :: Env -> String -> IO Bool
exists ref name = M.member name <$> readIORef ref

und :: String -> IOThrowsError TPLValue
und = throwError . UndefinedVariable

update :: Env -> String -> TPLValue -> IOThrowsError TPLValue
update ref name val = val <$ (liftIO $ M.insert name val <$> readIORef ref >>= writeIORef ref)

get :: Env -> String -> IOThrowsError TPLValue
get ref name = M.lookup name <$> liftIO (readIORef ref) >>= maybe (und name) return 

set :: Env -> String -> TPLValue -> IOThrowsError TPLValue
set ref name val = do env <- liftIO $ readIORef ref
                      maybe (und name) (\_->val <$ update ref  name val) $ M.lookup name env

define :: Env -> String -> TPLValue -> IOThrowsError TPLValue
define ref name val = set ref name val <|> update ref name val

bindVars :: Env -> [(String, TPLValue)] -> IO Env
bindVars ref bindings = M.union (M.fromList bindings) <$> readIORef ref >>= newIORef

defaultPrecedence :: TPLValue
defaultPrecedence = Number 10

getPrecedence :: Env -> String -> IOThrowsError TPLValue
getPrecedence ref op = (get ref $ "precedenceOf" ++ op) <|> return defaultPrecedence

setPrecedence :: Env -> String -> Integer -> IOThrowsError TPLValue
setPrecedence ref op precedence = define ref ("precedenceOf" ++ op) $ Number precedence
