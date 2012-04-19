module TPL.Eval (baseEnv, eval, evalString) where

import Control.Applicative
import Control.Monad.Error

import Data.Map (fromList)
import Data.IORef

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
  where evalExpr (Expression (fn@(Function{}) : args)) = apply env fn args
        evalExpr (Expression ((Native name) : args))   = native env name args
        evalExpr (Expression (fn : rest))              = do res <- eval env fn
                                                            evalExpr $ Expression (res : rest)
        evalExpr value                                 = eval env value
eval env (List vals)        = List <$> mapM (eval env) vals
eval _   (Sequence [])      = return Null
eval env (Sequence vals)    = last <$> mapM (eval env) vals
eval env (Lambda args body) = return $ Function env args body
eval env (ObjLit bindings)  = do ref <- newIORef . fromList <$> mapM evalBinding bindings
                                 Env <$> liftIO ref
  where evalBinding (s,v) = (,) s <$> eval env v
eval _ val                  = return val

apply :: Env -> TPLValue -> [TPLValue] -> IOThrowsError TPLValue
apply env fn@(Function closure params body) args 
  | length args < length params = Function closure newParams <$> newBody
  | otherwise = eArgs >>= liftIO . bindVars closure . unify params >>= (`eval` body)
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
           ("with", _with), ("in", _in),
           ("precedence", _precedence), ("precedenceOf", _precedenceOf),
           ("define", _define), ("set", _set), ("getFrom", _getFrom)] ++ map eager eagerNatives
  where eagerRight (name, op) = (name, \ env (left:rest) -> do strict <- mapM (eval env) rest
                                                               op env $ left:strict)
        eager (name, op)      = (name, \ env args -> mapM (eval env) args >>= op env)

load :: TPLOperation
load env [arg] = do filename     <- liftThrows $ toString arg >>= extract
                    path         <- eval env (Id "TPL_PATH") >>= liftThrows . (extract <=< toString)
                    let filepath = path ++ "/" ++ filename
                    List current <- eval env $ Id "_modules"
                    set env "_modules" . List $ (String filename) : current
                    run $ filepath ++ ".tpl"
  where run file = liftIO (readFile file) >>= liftThrows . readExpr >>= eval env
load _ expr    = throwError $ BadNativeCall "load" expr
        
require :: TPLOperation
require env [file] = do List current <- eval env $ Id "_modules"
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

_define :: TPLOperation
_define env [expr, val] = do name <- extractId env expr
                             eval env val >>= define env name 
_define _ args          = throwError $ BadNativeCall "define" args
                              
_set :: TPLOperation
_set env [expr, val] = do name <- extractId env expr
                          eval env val >>= set env name
_set _ args          = throwError $ BadNativeCall "set" args

_getFrom curr [Env env, expr] = get env =<< extractId env =<< eval curr expr
_getFrom env [envExpr, expr]  = do newEnv <- eval env envExpr
                                   _getFrom env [newEnv, expr]
_getFrom _ args               = throwError $ BadNativeCall "getFrom" args

_in :: TPLOperation
_in _ [Env env, expr]   = eval env expr
_in env [envExpr, expr] = do newEnv <- eval env envExpr
                             _in env [newEnv, expr]
_in _ args              = throwError $ BadNativeCall "in" args

_with :: TPLOperation
_with _ [Env env, Function _ args body] = return $ Function env args body
_with env [envExpr, funExpr]            = do newEnv <- eval env envExpr
                                             fun    <- eval env funExpr
                                             _with env [newEnv, fun]
_with _ args                            = throwError $ BadNativeCall "with" args

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
                                          
_precedence :: TPLOperation
_precedence env [opExpr, expr] = go [squash opExpr, expr]
  where go [Operator op, precedenceExpr] = go [Id op, precedenceExpr]
        go [Id op, precedenceExpr] = 
          do precedence <- eval env precedenceExpr >>= liftThrows . extract
             setPrecedence env op precedence
             return $ Number precedence          
        go badExpr =  throwError $ BadNativeCall "setPrecedence" badExpr
_precedence _ expr = throwError $ BadNativeCall "setPrecedence" expr
     
_precedenceOf :: TPLOperation
_precedenceOf env [Id op]              = getPrecedence env op
_precedenceOf env [Sequence [(Id op)]] = getPrecedence env op
_precedenceOf _ expr                   = throwError $ BadNativeCall "precedenceOf" expr
 
baseEnv :: IO Env
baseEnv = nullEnv >>= (`bindVars` map (\(name, _) -> (name, Native name)) natives)
                  >>= (`bindVars` [("_modules", List [])])

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ do res <- squash <$> liftThrows (readExpr expr) >>= eval env
                                       case res of
                                         Env _ -> return "<env>"
                                         _     -> return $ show res
