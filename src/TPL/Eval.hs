module TPL.Eval (baseEnv, eval, evalString) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Error

import Data.Function
import Data.List
import Data.Maybe

import TPL.Coerce
import TPL.Env
import TPL.Error
import TPL.Native
import TPL.Parse
import TPL.Pattern
import TPL.Value

readExp :: String -> ThrowsError TPLValue
readExp exp = case parse expressions "TPL" exp of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: Env -> TPLValue -> IOThrowsError TPLValue
eval env (If (Boolean condition) consequent alternate) = 
  if condition then eval env consequent else eval env alternate
eval env (If condition consequent alternate) = 
  do condVal <- eval env condition >>= liftThrows . toBool
     eval env $ If condVal consequent alternate
eval env (Id id) = do res <- get env id
                      case res of
                        Function closure [] body -> eval closure body
                        _                        -> return res
eval env (Operator op) = get env op
eval env val@(Expression _) = liftThrows (handleInfix val) >>= evalExp env
  where func = return . Function env [(Id "α")] 
        evalExp env (Expression [op@(Operator _), right])     = func $ (Expression [(Id "α"), op, right])
        evalExp env (Expression [left, op@(Operator _)])      = func $ (Expression [left, op, (Id "α")])
        evalExp env (Expression [a, Operator op, b])          = evalExp env $ Expression [(Id op), a, b]
        evalExp env (Expression (fn@(Function _ _ _) : args)) = apply env fn args
        evalExp env (Expression ((Native name) : args))       = native env name args
        evalExp env (Expression (first : rest))               = do res <- eval env first
                                                                   evalExp env $ Expression (res : rest)
        evalExp env val                                       = eval env val
eval env (List vals)     = List <$> mapM (eval env) vals
eval env (Sequence vals) = Expression . return . last <$> mapM (eval env) vals
eval env (Lambda args body) = return $ Function env args body
eval env val             = return val

apply :: Env -> TPLValue -> [TPLValue] -> IOThrowsError TPLValue
apply env fn@(Function closure params body) args 
  | length args < length params = Function closure newParams <$> newBody
  | otherwise                   = eArgs >>= liftIO . bindVars closure . unify params >>= (`eval` body)
    where eArgs = mapM (eval env) args
          newParams = map (Id . ("α" ++) . show) [1..length params - length args]
          newBody   = do args <- eArgs
                         return . Expression $ (fn : args) ++ newParams

isOp (Operator _) = True
isOp _            = False

handleInfix :: TPLValue -> ThrowsError TPLValue
handleInfix (Expression exp) =
  squash . Expression <$> (foldl1 (.) handleAll $ return exp')
  where exp' = map (squash . Expression) $ groupBy ((==) `on` isOp) exp
        handleAll = map ((=<<) . handle) operatorPrecedences
        handle :: Int -> [TPLValue] -> ThrowsError [TPLValue]
        handle precedence vals@(left : op@(Operator opStr) : right : more)
          | precedenceOf opStr == precedence =
            handle precedence $ (Expression [left, op, right]) : more
          | otherwise = fmap (left:) $ handle precedence (op:right:more)
        handle precedence (left:more) = fmap (left:) $ handle precedence more
        handle precedence val         = return val
handleInfix value = return value

precedenceOf :: String -> Int
precedenceOf = fromMaybe 10 . (`lookup` operatorPrecedence)

operatorPrecedences = [11,10..0]
operatorPrecedence = [("+", 5), ("-", 5),
                      ("*", 4), ("/", 4), ("><", 6),
                      ("=", 7), ("/=", 7), (">", 7), ("<", 7),
                      ("<=", 7), (">=", 7), ("|", 8), ("&", 8),
                      (":", 9), ("!", 9),
                      (":=", 11), ("<-", 11)]

-- Native functions:
native :: Env -> String -> [TPLValue] -> IOThrowsError TPLValue
native env name args = case lookup name natives of
  Just fn -> fn env args
  Nothing -> throwError . UndefinedVariable $ name ++ " <native>"
  
natives :: [(String, Env -> [TPLValue] -> IOThrowsError TPLValue)]
natives = [(":=", defineOp), eagerRight ("<-", setOp), eager ("load", load), ("with", with)] ++ 
          map eager eagerNatives
  where eagerRight (name, op) = (name, \ env (left:rest) -> do strict <- mapM (eval env) rest
                                                               safe op env $ left:strict)
        eager (name, op)      = (name, \ env args -> mapM (eval env) args >>= safe op env)

safe :: TPLOperation -> TPLOperation
safe op = op

load :: TPLOperation
load env args = do args    <- mapM (liftThrows <<< extract <=< toString) args
                   results <- mapM (run . (++ ".tpl")) args
                   return $ case results of 
                     [] -> Null
                     ls -> head ls
  where run file = liftIO (readFile file) >>= liftThrows . readExp >>= eval env

defineOp :: TPLOperation
defineOp env [Id name, val] = eval env val >>= define env name 
defineOp env [Expression [left, Operator op, right], body] =
  define env op $ Function env [left, right] body
defineOp env [Expression ((Id fn):args), body] = define env fn $ Function env args body
defineOp env [List vals, body]      = do res <- eval env body
                                         definePattern env [List vals, squash res]

definePattern :: TPLOperation
definePattern env [List vals, List body] = do mapM defPair $ unify vals body
                                              return $ List body
  where defPair (name, val) = define env name val
definePattern env [List vals, body] = defineOp env [List vals, List [body]]

  
setOp :: TPLOperation
setOp env ls@[Id _, _]           = set env ls
setOp env [List vals, List body] = do mapM setPair $ unify vals body
                                      return $ List body
  where setPair (name, val) = set env [Id name, val]
setOp env [List vals, body]      = setOp env [List vals, List [body]]

with :: TPLOperation
with env [(List bindings), body] = do let vars = mapM (toBinding . squash) bindings
                                      res <- vars >>= liftIO . bindVars env
                                      eval res body
  where toBinding (List [name, val]) = do val <- eval env val
                                          return (show name, val)
 

baseEnv = nullEnv >>= (`bindVars` map (\(name, _) -> (name, Native name)) natives)

evalString :: Env -> String -> IO String
evalString env exp = runIOThrows . liftM show $
                     squash <$> liftThrows (readExp exp) >>= eval env