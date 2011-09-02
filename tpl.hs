{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.Error
import Data.IORef
import Data.Maybe
import Debug.Trace
import IO hiding (try)
import List
import System.Environment
import Text.ParserCombinators.Parsec hiding (State)

-- Data types:
data TPLValue = Null
              | Id String
              | Number Int
              | String String
              | Boolean Bool
              | Operator String
              | List [TPLValue]
              | Expression [TPLValue]
              | Sequence [TPLValue]
              | Function [TPLValue] TPLValue
              | If TPLValue TPLValue TPLValue

showSeq :: [TPLValue] -> String
showSeq [] = ""
showSeq vals = foldl1 ((++) . (++ " ")) $ map show vals

instance Show TPLValue where
  show (Null) = "null"
  show (Id id) = id
  show (String str) = str
  show (Number int) = show int
  show (Operator name) = name
  show (Boolean bool) = show bool
  show (List vals) = show vals
  show (Expression vals) = showSeq vals
  show (Sequence vals) = unlines $ map show vals
  show (Function params body) =
    "λ " ++ showSeq params ++ " → {" ++ show body ++ "}"
  show (If condition consequent alternate) = "{?if " ++ show condition ++
                                             " then " ++ show consequent ++
                                             " else " ++ show alternate ++ "?}"
-- Errors:
data TPLError = Parser ParseError
              | BadOp String
              | MissingOperand String
              | TypeMismatch String String
              | UndefinedVariable String
              | Default String

showTPLE :: TPLError -> String
showTPLE (Parser err) = show err
showTPLE (BadOp op) = "Unknown operator " ++ op
showTPLE (Default str) = str
showTPLE (TypeMismatch expected got) = "Wrong type to operator. Expected " ++
                                       expected ++ "; got " ++ got
showTPLE (MissingOperand op) = "Missing operand for " ++ op
showTPLE (UndefinedVariable var) = "Variable " ++ var ++ " is undefined"

instance Show TPLError where
  show err = "Error: " ++ showTPLE err ++ "."
instance Error TPLError where
  noMsg = Default "An error has occured!"
  strMsg = Default

type ThrowsError = Either TPLError
type IOThrowsError = ErrorT TPLError IO

trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- Variables and environments:
type Env = IORef [(String, IORef TPLValue)]

nullEnv :: IO Env
nullEnv = newIORef []

existsVar :: Env -> String -> IO Bool
existsVar env name = liftM (maybe False (const True) . lookup name) $
                     readIORef env

getVar :: Env -> String -> IOThrowsError TPLValue
getVar env name = 
  do env <- liftIO $ readIORef env
     maybe (throwError $ UndefinedVariable name)
           (liftIO . readIORef)
           (lookup name env)
       
setVar :: Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue
setVar env (Id name) val = 
  do env <- liftIO $ readIORef env
     maybe (throwError $ UndefinedVariable name)
           (liftIO . (`writeIORef` val))
           (lookup name env)
     return val

defineVar :: Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue
defineVar env id@(Id name) val = 
  do exists <- liftIO $ existsVar env name
     if exists then setVar env id val >> return val
       else liftIO $ do valContents <- newIORef val
                        envContents <- readIORef env
                        writeIORef env $ (name, valContents) : envContents
                        return val

bindVars :: Env -> [(String, TPLValue)] -> IO Env
bindVars env bindings = readIORef env >>= extend bindings >>= newIORef
  where extend bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (name, val) = do ref <- newIORef val
                                    return (name, ref)
-- Parsing:
whiteSpace :: CharParser st ()
whiteSpace = spaces

terminator :: CharParser st Char
terminator = oneOf ";"

lexeme :: CharParser st a -> CharParser st a
lexeme p = do res <- p
              whiteSpace
              return res

idChar = letter <|> digit <|> oneOf "_"

keyWord :: String -> CharParser st String
keyWord str = do str <- try . lexeme $ string str
                 notFollowedBy idChar
                 return str

specChar :: CharParser st Char
specChar = fmap spec (oneOf "\"\\nt'"
                      <?> "valid escape character (\", n, t, \\, or ')")
  where spec char = case char of
          '"'  -> '"'
          'n'  -> '\n'
          't'  -> '\t'
          '\\' -> '\\'
          '\'' -> '\''
    
stringLiteral :: Parser TPLValue
stringLiteral = do opener <- oneOf "\"'"
                   contents <- (many $ (char '\\' >> specChar) <|> noneOf [opener])
                   char opener
                   return $ String contents

bool :: Parser TPLValue
bool = fmap (Boolean . (== "true")) $
       keyWord "true" <|> keyWord "false"

number :: Parser TPLValue
number = (fmap (Number . read) $ many1 digit)
         <?> "number"
               
identifier :: Parser TPLValue
identifier = do head <- letter <|> char '_'
                contents <- many $ idChar
                return . Id $ head:contents

operator :: Parser TPLValue
operator = fmap Operator $ many1 (oneOf "+-=*&^%$#@!?/.|~<>:") 

list :: Parser TPLValue
list = fmap List $ between (char '[') (char ']') $
       expression `sepBy` (char ',' >> spaces)

lambda :: Parser TPLValue
lambda = do oneOf "\\λ"
            parameters <- parameterList
            lexeme $ string "->"
            body <- optBlock
            return $ Function parameters body
  where parameterList = do whiteSpace
                           many $ lexeme identifier

expression :: Parser TPLValue
expression = fmap Expression $ many1 atom

block :: Parser TPLValue
block = fmap Sequence $
        between (char '{') (char '}') $
        expression `sepEndBy` lexeme terminator

optBlock :: Parser TPLValue
optBlock = lexeme (try block <|> expression)

ifStatement :: Parser TPLValue
ifStatement = try $ do keyWord "if"
                       condition <- lexeme parenExp
                       (consequent, alternate) <- blockElse <|> expElse <|> ifOptBlock
                       return $ If condition consequent alternate
  where ifOptBlock = do body <- optBlock
                        return (body, Null)
        blockElse = try $ do consequent <- block
                             keyWord "else"
                             alternate <- optBlock
                             return (consequent, alternate)
        expElse = try $ do consequent <- lexeme $ manyTill atom (try $ string "else")
                           alternate <- optBlock
                           return (Expression consequent, alternate)
        

parenExp :: Parser TPLValue
parenExp = between (char '(') (char ')') expression

atom :: Parser TPLValue
atom = lexeme token
  where token = lambda
            <|> ifStatement
            <|> bool
            <|> identifier
            <|> stringLiteral
            <|> number
            <|> operator
            <|> list
            <|> parenExp
            <|> block

expressions :: Parser TPLValue
expressions = fmap Sequence $ expression `sepEndBy` lexeme terminator

readExp :: String -> ThrowsError TPLValue
readExp exp = case parse expressions "TPL" exp of
  Left err -> throwError $ Parser err
  Right val -> return val

eval :: Env -> TPLValue -> IOThrowsError TPLValue
eval env (If (Boolean condition) consequent alternate) = 
  if condition then eval env consequent else eval env alternate
eval env (If condition consequent alternate) = 
  do condVal <- eval env condition
     eval env $ If condVal consequent alternate
eval env (Id id) = getVar env id
eval env val@(Expression _) = liftThrows (handleInfix val) >>= evalExp env
  where evalExp env (Expression [a, (Operator op), b]) = (operate op) env a b
        evalExp env (Expression (id@(Id _) : rest)) = 
          do res <- eval env id
             evalExp env $ Expression (res : rest)
        evalExp env (Expression (fn@(Function _ _) : args)) = apply env fn args
        evalExp env val = eval env val
eval env (List vals) = liftM List $ mapM (eval env) vals
eval env (Sequence vals) = liftM (Expression . return . last) $ mapM (eval env) vals
eval env val = return val
  
apply :: Env -> TPLValue -> [TPLValue] -> IOThrowsError TPLValue
apply env (Function params body) args = 
  do args <- mapM (eval env) args
     newEnv <- liftIO $ bindVars env $ zip (map show params) args
     eval newEnv body

squash :: TPLValue -> TPLValue
squash (Expression [val]) = val
squash val = val

handleInfix :: TPLValue -> ThrowsError TPLValue
handleInfix (Expression exp) =
  fmap (squash . Expression) $ foldl1 (.) handleAll $ return $ exp
  where handleAll = map (flip (>>=) . (handle)) operatorPrecedences
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

eagerLeft :: (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue) -> 
             (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
eagerLeft op env l r = do lVal <- eval env l
                          op env lVal r
                       
eagerRight :: (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue) -> 
              (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
eagerRight op env l r = do rVal <- eval env r
                           op env l rVal
                           
eager = eagerLeft . eagerRight

operators = [("+", eager $ numericBinOp (+)), ("-", eager $ numericBinOp (-)), 
             ("*", eager $ numericBinOp (*)), ("/", eager $ numericBinOp div), 
             ("><", eager $ strBinOp (++)),                                    
             ("==", eager $ boolBinOp (==)), ("!=", eager $ boolBinOp (/=)),
             (":", eager cons), ("!!", eager index), ("..", eager range),
             ("=", eagerRight defineVar)]

precedenceOf :: String -> Int
precedenceOf = fromMaybe 0 . (`lookup` operatorPrecedence)

operatorPrecedences = [10,9..0]
operatorPrecedence = [("+", 5), ("-", 5),
                      ("*", 4), ("/", 4), ("><", 6),
                      ("==", 8), ("!=", 8), 
                      (":", 9), ("!!", 9), ("..", 9),
                      ("=", 10)]

operate :: String -> Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue
operate op env left right = case (lookup op operators) of
  Just fn -> fn env left right
  Nothing -> liftThrows $ throwError $ BadOp op

-- Type coercion:
type TPLOperation = (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
type Coercer = (TPLValue -> ThrowsError TPLValue)

-- Takes a function on TPLValues and makes it coerce to the given type.
class Extractable a where extract :: TPLValue -> ThrowsError a
class Packable a where pack :: a -> TPLValue
  
instance Extractable Int where
  extract (Number n) = return n
  extract num = do n <- toNumber num
                   extract n
instance Packable Int where pack = Number
                            
instance Extractable [Char] where extract = return . show
instance Packable [Char] where pack = String

liftOp :: (Extractable a, Extractable b, Packable c) => (a -> b -> c) -> TPLOperation
liftOp op = \ env a b ->
  do av <- liftThrows $ extract a
     bv <- liftThrows $ extract b
     return $ pack $ op av bv
                       
                       
coercable :: TPLOperation -> Coercer -> Coercer -> TPLOperation
coercable fn coerce1 coerce2 env arg1 arg2 = do val1 <- liftThrows $ coerce1 arg1
                                                val2 <- liftThrows $ coerce2 arg2
                                                fn env val1 val2

toNumber :: TPLValue -> ThrowsError TPLValue
toNumber num@(Number _) = return num
toNumber (String str) = return $ Number $ read str
toNumber (List []) = throwError $ TypeMismatch "Number" (show $ List [])
toNumber (List (val:_)) = toNumber val
toNumber val = throwError $ TypeMismatch "Number" $ show val

toString :: TPLValue -> ThrowsError TPLValue
toString = (fmap String) . extract

numericBinOp :: (Int -> Int -> Int) -> TPLOperation
numericBinOp op = coercable (liftOp op) toNumber toNumber

strBinOp :: (String -> String -> String) -> TPLOperation
strBinOp op = coercable (liftOp op) toString toString

cons :: Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue
cons env head (List tail) = return $ List $ head : tail
cons env head tail = return $ List $ head : [tail]

index :: Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue
index env (List list) (Number i) = return $ list !! i
index env (List list) (String str) = return $ list !! read str
index env val i = index env (List [val]) i

range :: Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue
range env (Number start) (Number end) = return $ List $ map Number [start..end]

boolBinOp :: (String -> String -> Bool) ->
             (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
boolBinOp op _ left right = 
  liftThrows $ return $ Boolean $ op (show left) (show right)

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
repl = nullEnv >>= until_ (== "quit") (readPrompt "~>") . evalAndPrint 

runFile :: FilePath -> IO ()
runFile path = do code <- readFile path
                  nullEnv >>= flip evalAndPrint code

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> repl
            1 -> runFile $ args !! 0
            otherwise -> putStrLn "Too many arguments!"