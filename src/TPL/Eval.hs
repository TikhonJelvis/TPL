module TPL.Eval (baseEnv, eval, evalString) where

import Control.Applicative
import Control.Monad.Error

import TPL.Coerce
import TPL.Env
import TPL.Error
import TPL.Native
import TPL.Parse
import TPL.Pattern
import TPL.Syntax
import TPL.Value

readExpr :: String -> ThrowsError TPLValue
readExpr expr = case parse expressions "TPL" expr of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: Env -> TPLValue -> IOThrowsError TPLValue
eval env (Id name) = do res <- get env name
                        case res of
                          Lambda [] body           -> eval env body
                          Function closure [] body -> eval closure body
                          expr                     -> eval env expr
eval env val@(Expression _) = normalize env val >>= evalExpr
  where evalExpr (Expression (fn@(Function{}) : args))     = apply env fn args
        evalExpr (Expression ((Native name) : args))       = native env name args
        evalExpr (Expression (fn : rest))                  = do res <- eval env fn
                                                                evalExpr $ Expression (res : rest)
        evalExpr value                                     = eval env value
eval env (List vals)        = List <$> mapM (eval env) vals
eval _   (Sequence [])      = return Null
eval env (Sequence vals)    = last <$> mapM (eval env) vals
eval env (Lambda args body) = return $ Function env args body
eval _ val                  = return val

apply :: Env -> TPLValue -> [TPLValue] -> IOThrowsError TPLValue
apply env fn@(Function closure params body) args 
  | length args < length params = Function closure newParams <$> newBody
  | otherwise                   = eArgs >>= liftIO . bindVars closure . unify params >>= (`eval` body)
    where eArgs = mapM conditionallyEval $ zip params args
          conditionallyEval (Lambda{}, arg) = return $ Function env [] arg
          conditionallyEval (_, arg)        = eval env arg
          newParams = map getName $ zip [1..length params - length args] (drop (length args) params)
          getName (number, Lambda{}) = Lambda [] . Id . ("α" ++) $ show number
          getName (number, _)        = Id . ("α" ++) $ show number
          newBody   = do resArgs <- eArgs
                         return . Expression $ (fn : resArgs) ++ newParams
apply _ notFn _ = throwError $ TypeMismatch "function" $ show notFn

-- Native functions:
native :: Env -> String -> [TPLValue] -> IOThrowsError TPLValue
native env name args = case lookup name natives of
  Just fn -> fn env args
  Nothing -> throwError . UndefinedVariable $ name ++ " <native>"
  
natives :: [(String, TPLOperation)]
natives = [(":=", defineOp), eagerRight ("<-", setOp), eager ("load", load), ("require", require), 
           ("with", with), ("precedence", setPrecedenceOp), ("precedenceOf", getPrecedenceOp),
           ("define", _define), ("set", _set), ("get", _get)] ++ map eager eagerNatives
  where eagerRight (name, op) = (name, \ env (left:rest) -> do strict <- mapM (eval env) rest
                                                               op env $ left:strict)
        eager (name, op)      = (name, \ env args -> mapM (eval env) args >>= op env)

load :: TPLOperation
load env [arg] = do filename     <- liftThrows $ toString arg >>= extract
                    path         <- get env "TPL_PATH" >>= liftThrows . (extract <=< toString)
                    let filepath = path ++ "/" ++ filename
                    List current <- get env "_modules"
                    set env "_modules" . List $ (String filename) : current
                    run $ filepath ++ ".tpl"
  where run file = liftIO (readFile file) >>= liftThrows . readExpr >>= eval env
load _ expr    = throwError $ BadNativeCall "load" expr
        
require :: TPLOperation
require env [file] = do List current <- get env "_modules"
                        String name <- liftThrows $ toString file
                        if String name `elem` current
                          then return Null else load env [file]
require _ expr = throwError $ BadNativeCall "require" expr
                        
defineOp :: TPLOperation
defineOp env [Id name, val] = eval env val >>= define env name 
defineOp env [Expression [left, Operator op, right], body] =
  define env op $ Function env [left, right] body
defineOp env [Expression ((Id fn):args), body] = define env fn $ Function env args body
defineOp env [List vals, body]      = do res <- eval env body
                                         definePattern env [List vals, squash res]
defineOp _ expr = throwError . Default $ "Invalid definition: " ++ show expr
                                         
extractId :: Env -> TPLValue -> IOThrowsError String
extractId env (Id name)                 = do res <- get env name
                                             case res of
                                               Id str -> return str
                                               _      -> extractId env res
extractId _ (String str)              = return str
extractId _ (Lambda [] (Id name))     = return name 
extractId _ (Function _ [] (Id name)) = return name
extractId _ val                       = throwError . Default $ "Invalid variable: " ++ show val

_get :: TPLOperation
_get env [expr] = do name <- extractId env expr
                     get env name
_get _ args     = throwError $ BadNativeCall "get" args

_define :: TPLOperation
_define env [expr, val] = do name <- extractId env expr
                             eval env val >>= define env name 
_define _ args          = throwError $ BadNativeCall "define" args
                              
_set :: TPLOperation
_set env [expr, val] = do name <- extractId env expr
                          eval env val >>= set env name
_set _ args          = throwError $ BadNativeCall "set" args

with :: TPLOperation
with env withArgs@[List bindings, Function closure args body] = 
  do res <- mapM (toBinding . squash) bindings >>= liftIO . bindVars closure
     apply env (Function res args body) []
  where toBinding (Expression [Id "->", name, val]) = toBinding $ List [name, val]
        toBinding (List [nameExp, val]) = do evalled  <- eval env val
                                             name <- extractId env nameExp
                                             return (name, evalled)
        toBinding _ = throwError $ BadNativeCall "with" withArgs
with env [bindings, Id name] = do val <- get env name
                                  with env [bindings, val]
with env [Id name, expr]      = do bindings <- get env name
                                   with env [bindings, expr]
with env [Function _ [] b, expr] = with env [b, expr]
with env [bindings, expr]   = do evaluated <- eval env expr
                                 with env [bindings, evaluated]
with _ args                 = throwError $ BadNativeCall "with" args

definePattern :: TPLOperation
definePattern env [List vals, List body] = do mapM_ defPair $ unify vals body
                                              return $ List body
  where defPair (name, val) = define env name val
definePattern env [List vals, body] = defineOp env [List vals, List [body]]
definePattern _ args = throwError $ BadNativeCall "define" args

setOp :: TPLOperation
setOp env [Id name, val]         = set env name val
setOp env [List vals, List body] = do mapM_ setPair $ unify vals body
                                      return $ List body
  where setPair (name, val) = set env name val
setOp env [List vals, body] = setOp env [List vals, List [body]]
setOp _ expr                = throwError $ BadNativeCall "set" expr
                                          
setPrecedenceOp :: TPLOperation
setPrecedenceOp env [Id op, precedenceExpr] = 
  do precedence <- eval env precedenceExpr >>= liftThrows . extract
     setPrecedence env op precedence
     return $ Number precedence
setPrecedenceOp _ expr = throwError $ BadNativeCall "setPrecedence" expr
     
getPrecedenceOp :: TPLOperation
getPrecedenceOp env [Id op] = getPrecedence env op
getPrecedenceOp _ expr = throwError $ BadNativeCall "precedenceOf" expr
 
baseEnv :: IO Env
baseEnv = nullEnv >>= (`bindVars` map (\(name, _) -> (name, Native name)) natives)
                  >>= (`bindVars` [("_modules", List [])])

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows . liftM show $
                     squash <$> liftThrows (readExpr expr) >>= eval env
