{-# LANGUAGE FlexibleInstances #-}
module TPL.Run where
 
import Control.Applicative
import Control.Monad.Error

import Data.IORef
import Data.List
import Data.Maybe

import System.Environment
import System.IO

import Text.ParserCombinators.Parsec hiding (State)

import TPL.Parse

-- Errors:
data TPLError = Parser ParseError
              | BadOp String
              | MissingOperand String
              | TypeMismatch String String
              | UndefinedVariable String
              | Default String

showTPLE :: TPLError -> String
showTPLE (Parser err)                = show err
showTPLE (BadOp op)                  = "Unknown operator " ++ op
showTPLE (Default str)               = str
showTPLE (TypeMismatch expected got) = "Wrong type. Expected " ++ expected ++ "; got " ++ got ++ "."
showTPLE (MissingOperand op)         = "Missing operand for " ++ op
showTPLE (UndefinedVariable var)     = "Variable " ++ var ++ " is undefined"

instance Show TPLError where
  show err = "Error: " ++ showTPLE err ++ "."
instance Error TPLError where
  noMsg = Default "An error has occured!"
  strMsg = Default

type ThrowsError = Either TPLError
type IOThrowsError = ErrorT TPLError IO

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runErrorT (trapError action)

-- Variables and environments:
type Env = IORef [(String, IORef TPLValue)]

baseEnv = newIORef [] >>= (`bindVars` map (\(name, _) -> (name, Native name)) natives)

existsVar :: Env -> String -> IO Bool
existsVar env name = isJust . lookup name <$> readIORef env

getVar :: Env -> String -> IOThrowsError TPLValue
getVar env name = do env <- liftIO $ readIORef env
                     case lookup name env of
                       Just ref -> liftIO $ readIORef ref
                       Nothing  -> throwError $ UndefinedVariable name
       
setVar :: Env -> [TPLValue] -> IOThrowsError TPLValue
setVar env [(Id name), val] = do env <- liftIO $ readIORef env
                                 case lookup name env of
                                   Just ref -> liftIO $ writeIORef ref val >> return val
                                   Nothing  -> throwError $ UndefinedVariable name

defineVar :: Env -> [TPLValue] -> IOThrowsError TPLValue
defineVar env [id@(Id name), val] = liftIO (existsVar env name) >>= define >> return val
  where define True  = setVar env [id, val] >> return ()
        define False = liftIO $ do value   <- newIORef val
                                   currEnv <- readIORef env
                                   writeIORef env $ (name, value) : currEnv 

bindVars :: Env -> [(String, TPLValue)] -> IO Env
bindVars env bindings = readIORef env >>= extend bindings >>= newIORef
  where extend bindings env = (++ env) <$> mapM addBinding bindings
        addBinding (name, val) = newIORef val >>= \ ref -> return (name, ref)
                                 
readExp :: String -> ThrowsError TPLValue
readExp exp = case parse expressions "TPL" exp of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: Env -> TPLValue -> IOThrowsError TPLValue
eval env (If (Boolean condition) consequent alternate) = 
  if condition then eval env consequent else eval env alternate
eval env (If condition consequent alternate) = 
  do condVal <- eval env condition
     eval env $ If condVal consequent alternate
eval env (Id id) = getVar env id
eval env val@(Expression _) = liftThrows (handleInfix val) >>= evalExp env
  where evalExp env (Expression [a, (Operator op), b])      = native env op [a,b]
        evalExp env (Expression (id@(Id name) : rest))      = do res <- eval env id
                                                                 evalExp env $ Expression (res : rest)
        evalExp env (Expression (fn@(Function _ _) : args)) = apply env fn args
        evalExp env (Expression ((Native name) : args))     = native env name args
        evalExp env val                                     = eval env val
eval env (List vals)     = List <$> mapM (eval env) vals
eval env (Sequence vals) = Expression . return . last <$> mapM (eval env) vals
eval env val             = return val

native :: Env -> String -> [TPLValue] -> IOThrowsError TPLValue
native env name args = case lookup name natives of
  Just fn -> fn env args
  Nothing -> throwError . UndefinedVariable $ name ++ " <native>"
  
natives :: [(String, Env -> [TPLValue] -> IOThrowsError TPLValue)]
natives = map eagerRight [(":=", defineVar), ("<-", setVar)] ++ 
          map eager [("length", len), ("+", numOp (+)), ("-", numOp (-)),
           ("*", numOp (*)), ("/", numOp div), ("|", liftOp (||)), 
           ("&", liftOp (&&)), ("=", eqOp (==)), ("/=", eqOp (/=)),
           ("><", strOp (++)), (":", cons), ("!", index), ("..", range),
           ("head", \ _ [(List ls)] -> return $ head ls),
           ("tail", \ _ [(List ls)] -> return . List $ tail ls)]
  where numOp = liftOp :: (Int -> Int -> Int) -> TPLOperation
        eqOp  = liftOp :: (String -> String -> Bool) -> TPLOperation
        strOp = liftOp :: (String -> String -> String) -> TPLOperation
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

apply :: Env -> TPLValue -> [TPLValue] -> IOThrowsError TPLValue
apply env (Function params body) args = 
  mapM (eval env) args >>= liftIO . bindVars env . zip (map show params) >>= (`eval` body)

squash :: TPLValue -> TPLValue
squash (Expression [val]) = val
squash val = val

handleInfix :: TPLValue -> ThrowsError TPLValue
handleInfix (Expression exp) =
  squash . Expression <$> (foldl1 (.) handleAll $ return exp)
  where handleAll = map ((=<<) . handle) operatorPrecedences
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
precedenceOf = fromMaybe 0 . (`lookup` operatorPrecedence)

operatorPrecedences = [10,9..0]
operatorPrecedence = [("+", 5), ("-", 5),
                      ("*", 4), ("/", 4), ("><", 6),
                      ("=", 7), ("/=", 7), ("|", 8), ("&", 8),
                      (":", 9), ("!", 9), ("..", 9),
                      (":=", 10), ("<-", 10)]

-- Type coercion:
type TPLOperation = (Env -> [TPLValue] -> IOThrowsError TPLValue)
type Coercer = (TPLValue -> ThrowsError TPLValue)

-- Takes a function on TPLValues and makes it coerce to the given type.
class Extractable a where extract :: TPLValue -> ThrowsError a
class Packable a where pack :: a -> TPLValue
  
instance Extractable Int where
  extract (Number n) = return n
  extract num = toNumber num >>= extract
instance Packable Int where pack = Number
                            
instance Extractable [Char] where extract = return . show
instance Packable [Char] where pack = String

instance Extractable Bool where 
  extract (Boolean False) = return False
  extract _               = return True
instance Packable Bool where pack = Boolean

liftOp :: (Extractable a, Extractable b, Packable c) => (a -> b -> c) -> TPLOperation
liftOp op = \ env [a, b] ->
  do av <- liftThrows $ extract a
     bv <- liftThrows $ extract b
     return . pack $ op av bv
                       
coercable :: TPLOperation -> Coercer -> Coercer -> TPLOperation
coercable fn coerce1 coerce2 env [arg1, arg2] = do val1 <- liftThrows $ coerce1 arg1
                                                   val2 <- liftThrows $ coerce2 arg2
                                                   fn env [val1, val2]

toNumber :: TPLValue -> ThrowsError TPLValue
toNumber num@(Number _) = return num
toNumber (String str)   = return . Number $ read str
toNumber (List [])      = throwError . TypeMismatch "Number" . show $ List []
toNumber (List (val:_)) = toNumber val
toNumber val            = throwError . TypeMismatch "Number" $ show val

toString :: TPLValue -> ThrowsError TPLValue
toString = liftM String . extract

unpack :: ThrowsError TPLValue -> TPLValue
unpack (Right val) = val
unpack (Left err) = String $ show err

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: Env -> String -> IO String
evalString env exp = runIOThrows $ liftM show $ 
                     (fmap squash $ liftThrows $ readExp exp) >>= eval env
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env exp = evalString env exp >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = 
  do result <- prompt
     if pred result 
       then return ()
       else action result >> until_ pred prompt action

repl :: IO ()
repl = baseEnv >>= until_ (== "quit") (readPrompt "~>") . evalAndPrint 

runFile :: FilePath -> IO ()
runFile path = do code <- readFile path
                  baseEnv >>= flip evalAndPrint code

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> repl
            1 -> runFile $ args !! 0
            otherwise -> putStrLn "Too many arguments!"