module TPL.Env (get, set, define, bindVars, getPrecedence, setPrecedence) where

import Control.Applicative
import Control.Monad.Error

import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.IORef

import TPL.Coerce
import TPL.Error
import TPL.Value

und :: String -> IOThrowsError TPLValue
und = throwError . UndefinedVariable

update :: Env -> String -> TPLValue -> IOThrowsError TPLValue
update ref name val = val <$ (liftIO $ M.insert name val <$> readIORef ref >>= writeIORef ref)

getLocal :: Env -> String -> IOThrowsError TPLValue
getLocal ref name = M.lookup name <$> liftIO (readIORef ref) >>= maybe (und name) return

setLocal :: Env -> String -> TPLValue -> IOThrowsError TPLValue
setLocal ref name val = do maybeVal <- M.lookup name <$> liftIO (readIORef ref)
                           if isJust maybeVal then update ref name val else und name

get :: Env -> String -> IOThrowsError TPLValue
get ref "*current*" = return $ Env ref
get ref name = getLocal ref name <|> (getLocal ref "*parent*" >>= liftThrows . extract >>= (`get` name)) <|> und name

set :: Env -> String -> TPLValue -> IOThrowsError TPLValue
set _ "*current*" _ = throwError $ Default "Cannot change the current environment"
set ref name val = setLocal ref name val <|> do parent <- get ref "*parent*" >>= liftThrows . extract
                                                set parent name val

define :: Env -> String -> TPLValue -> IOThrowsError TPLValue
define ref name val = setLocal ref name val <|> update ref name val

bindVars :: Env -> [(String, TPLValue)] -> IO Env
bindVars ref bindings = let parent = pack ref
                            newEnv = M.insert "*parent*" parent $ M.fromList bindings in
                        newIORef newEnv

defaultPrecedence :: TPLValue
defaultPrecedence = Number 10

getPrecedence :: Env -> String -> IOThrowsError TPLValue
getPrecedence ref op = get ref ("precedenceOf" ++ op) <|> return defaultPrecedence

setPrecedence :: Env -> String -> Integer -> IOThrowsError TPLValue
setPrecedence ref op precedence = define ref ("precedenceOf" ++ op) $ Number precedence
