module TPL.Run where
 
import Control.Applicative
import Control.Arrow
import Control.Monad.Error

import Data.Function
import Data.List
import Data.Maybe

import System.Environment
import System.IO

import Text.ParserCombinators.Parsec hiding (State)

import TPL.Coerce
import TPL.Error
import TPL.Env
import TPL.Parse
                                 
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
eval env (Id id) = get env id
eval env val@(Expression _) = liftThrows (handleInfix val) >>= evalExp env
  where evalExp env (Expression [a, (Operator op), b])      = evalExp env $ Expression [(Id op), a, b]
        evalExp env (Expression (fn@(Function _ _) : args)) = apply env fn args
        evalExp env (Expression ((Native name) : args))     = native env name args
        evalExp env (Expression (first : rest))             = do res <- eval env first
                                                                 evalExp env $ Expression (res : rest)
        evalExp env val                                     = eval env val
eval env (List vals)     = List <$> mapM (eval env) vals
eval env (Sequence vals) = Expression . return . last <$> mapM (eval env) vals
eval env val             = return val

apply :: Env -> TPLValue -> [TPLValue] -> IOThrowsError TPLValue
apply env (Function params body) args = 
  mapM (eval env) args >>= liftIO . bindVars env . zip (map show params) >>= (`eval` body)

squash :: TPLValue -> TPLValue
squash (Expression [val]) = val
squash (Sequence [val])   = val
squash val                = val

isOp (Operator _) = True
isOp _            = False

handleInfix :: TPLValue -> ThrowsError TPLValue
handleInfix (Expression exp) =
  squash . Expression <$> (foldl1 (.) handleAll $ return exp')
  where exp' = map (squash . Expression) $ groupBy ((==) `on` isOp) exp
        handleAll = map ((=<<) . handle) operatorPrecedences
        handle :: Int -> [TPLValue] -> ThrowsError [TPLValue]
        handle _ [] = return []
        handle _ [(Operator op)] = throwError $ MissingOperand op
        handle _ [a] = return [a]
        handle precedence exp@[left, (Operator op)]
          | precedenceOf op == precedence = throwError $ MissingOperand op
          | otherwise = return exp
        handle precedence exp@[(Operator op), right]
          | precedenceOf op == precedence = throwError $ MissingOperand op
          | otherwise = return exp
        handle _ [a, b] = return [a, b]
        handle precedence vals@(left : op@(Operator opStr) : right : more)
          | precedenceOf opStr == precedence =
            handle precedence $ (Expression [left, op, right]) : more
          | otherwise = fmap (left:) $ handle precedence (op:right:more)
        handle precedence (left:more) = fmap (left:) $ handle precedence more
handleInfix value = return value

precedenceOf :: String -> Int
precedenceOf = fromMaybe 10 . (`lookup` operatorPrecedence)

operatorPrecedences = [11,10..0]
operatorPrecedence = [("+", 5), ("-", 5),
                      ("*", 4), ("/", 4), ("><", 6),
                      ("=", 7), ("/=", 7), ("|", 8), ("&", 8),
                      (":", 9), ("!", 9), ("..", 9),
                      (":=", 11), ("<-", 11)]

-- Native functions:
native :: Env -> String -> [TPLValue] -> IOThrowsError TPLValue
native env name args = case lookup name natives of
  Just fn -> fn env args
  Nothing -> throwError . UndefinedVariable $ name ++ " <native>"
  
natives :: [(String, Env -> [TPLValue] -> IOThrowsError TPLValue)]
natives = [(":=", defineOp), eagerRight ("<-", set)] ++ 
          map eager [("length", len), ("+", numOp (+)), ("-", numOp (-)),
           ("*", numOp (*)), ("/", numOp div), ("|", liftOp (||)), 
           ("&", liftOp (&&)), ("=", eqOp (==)), ("/=", eqOp (/=)),
           ("><", strOp (++)), (":", cons), ("!", index), ("..", range),
           ("head", \ _ [ls] -> return $ tplHead ls),
           ("tail", \ _ [ls] -> return . List $ tplTail ls), ("load", load)]
  where tplHead (List ls) = head ls
        tplHead val       = val
        tplTail (List ls) = tail ls 
        tplTail _         = []
        eagerRight (name, op) = (name, \ env (left:rest) -> do strict <- mapM (eval env) rest
                                                               op env $ left:strict)
        eager (name, op) = (name, \ env args -> mapM (eval env) args >>= op env)
        
len :: Env -> [TPLValue] -> IOThrowsError TPLValue
len _ [(List ls)] = return . Number $ length ls
len _ _           = return $ Number 1
        
cons :: Env -> [TPLValue] -> IOThrowsError TPLValue
cons env [head, (List tail)] = return . List $ head : tail
cons env [head, tail]        = return . List $ head : [tail]

-- TODO: Fix this to work with the whole coercion framework...
index :: Env -> [TPLValue] -> IOThrowsError TPLValue
index env [(List list), (Number i)]   = return $ list !! i
index env [(List list), (String str)] = return $ list !! read str
index env [val, i]                    = index env [(List [val]), i]

range :: Env -> [TPLValue] -> IOThrowsError TPLValue
range env [(Number start), (Number end)] = return . List $ map Number [start..end]
range env args = mapM (liftThrows . toNumber) args >>= range env

load :: Env -> [TPLValue] -> IOThrowsError TPLValue
load env args = do args    <- mapM (liftThrows <<< extract <=< toString) args
                   results <- mapM (run . (++ ".tpl")) args
                   return $ case results of 
                     [] -> Null
                     ls -> head ls
  where run file = liftIO (readFile file) >>= liftThrows . readExp >>= eval env

defineOp :: Env -> [TPLValue] -> IOThrowsError TPLValue
defineOp env [(Id name), val] = eval env val >>= define env name 
defineOp env [(Expression [left@(Id _), (Operator op), right@(Id _)]), body] =
  define env op $ Function [left, right] body
defineOp env [(Expression ((Id fn):args)), body] = define env fn $ Function args body

unpack :: ThrowsError TPLValue -> TPLValue
unpack (Right val) = val
unpack (Left err) = String $ show err

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: Env -> String -> IO String
evalString env exp = runIOThrows . liftM show $
                     squash <$> liftThrows (readExp exp) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env exp = evalString env exp >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = 
  do result <- prompt
     if pred result 
       then return ()
       else action result >> until_ pred prompt action

baseEnv = nullEnv >>= (`bindVars` map (\(name, _) -> (name, Native name)) natives)

repl :: IO ()
repl = baseEnv >>= until_ (== "quit") (readPrompt "~>") . evalAndPrint 

runFile :: FilePath -> IO ()
runFile path = do code <- readFile path
                  baseEnv >>= (`evalAndPrint` code)

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> repl
            ls -> mapM_ runFile ls