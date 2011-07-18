module Main where

import Control.Monad.Error
import Data.IORef
import Data.Maybe
import IO hiding (try)
import Text.ParserCombinators.Parsec hiding (State)

data TPLValue = Id String
              | Number Int
              | String String
              | Boolean Bool
              | Operator String
              | List [TPLValue]
              | Expression [TPLValue]
              | Function [TPLValue] TPLValue

showSeq :: [TPLValue] -> String
showSeq = foldl1 ((++) . (++ " ")) . (map show)

instance Show TPLValue where
  show (Id id) = id
  show (String str) = show str
  show (Number int) = show int
  show (Operator name) = name
  show (Boolean bool) = show bool
  show (List vals) = show vals
  show (Expression vals) = showSeq vals
  show (Function params body) =
    "λ " ++ showSeq params ++ " → {" ++ show body ++ "}"

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

type Env = IORef [(String, IORef TPLValue)]

nullEnv :: IO Env
nullEnv = newIORef []

existsVar :: Env -> String -> IO Bool
existsVar env name = readIORef env >>= return . maybe False (const True) . lookup name

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

specChar :: CharParser st Char
specChar = do char <- oneOf "\"\\nt'"
              return $ case char of
                '"'  -> '"'
                'n'  -> '\n'
                't'  -> '\t'
                '\\' -> '\\'
                '\'' -> '\''

parseString :: Parser TPLValue
parseString = do opener <- oneOf "\"'"
                 contents <- many $ (char '\\' >> specChar) <|> noneOf [opener]
                 char opener
                 return $ String contents

parseId :: Parser TPLValue
parseId = do head <- letter <|> char '_'
             contents <- many $ letter <|> digit <|> oneOf "_!?"
             return $ Id $ head:contents

parseOperator :: Parser TPLValue
parseOperator = many1 (oneOf "+-=*&^%$#@!?/.|~<>:") >>= return . Operator

parseNumber :: Parser TPLValue
parseNumber = many1 digit >>= return . Number . read

parseList :: Parser TPLValue
parseList = between (char '[') (char ']')
            (parseTPL `sepBy` (spaces >> char ',')) >>= return . List

parseLambda :: Parser TPLValue
parseLambda = do parameters <- between (oneOf "\\λ") (string "->")
                                      (id `sepBy` (spaces >> char ','))
                 body <- spaces >> parseExpression
                 return $ Function parameters body
  where id = do id <- spaces >> parseId
                spaces >> return id

parseExpression :: Parser TPLValue
parseExpression = many parseTPL >>= return . Expression

parseParenExp :: Parser TPLValue
parseParenExp = between (char '(') (char ')') parseExpression

parseTPL :: Parser TPLValue
parseTPL = spaces >> (parseLambda
                  <|> parseId
                  <|> parseString
                  <|> parseNumber
                  <|> parseOperator
                  <|> parseList
                  <|> parseParenExp)

parseExpressions :: Parser [TPLValue]
parseExpressions = parseExpression `sepBy` oneOf ";\n"

readExp :: String -> ThrowsError TPLValue
readExp exp = case parse parseExpressions "TPL" exp of
  Left err -> throwError $ Parser err
  Right val -> return $ Expression val

eval :: Env -> TPLValue -> IOThrowsError TPLValue
eval env (Id id) = getVar env id
eval env (Expression [val]) = eval env val
eval env (Expression [a, (Operator op), b]) = (operate op) env a b
eval env (Expression (id@(Id _) : rest)) = 
  do val <- eval env id
     eval env $ Expression $ val : rest
eval env val@(Expression _) = liftThrows (handleInfix val) >>= eval env
eval env val = return val

handleInfix :: TPLValue -> ThrowsError TPLValue
handleInfix (Expression exp) =
  foldl1 (.) handleAll (return exp) >>= return . Expression
  where
    handleAll = map (flip (>>=) . handle) operatorPrecedences
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
        handle precedence $ Expression [left, op, right] : more
      | otherwise = handle precedence (op:right:more) >>= return . (left:)
    handle precedence (left:more) = handle precedence more >>= return . (left:)
handleInfix value = return value

eagerLeft :: (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue) -> (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
eagerLeft op env l r = do lVal <- eval env l
                          op env lVal r
                       
eagerRight :: (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue) -> (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
eagerRight op env l r = do rVal <- eval env r
                           op env l rVal
                           
eager = eagerRight . eagerLeft

operators = [("+", eager $ numericBinOp (+)), ("-", eager $ numericBinOp (-)),
             ("*", eager $ numericBinOp (*)), ("/", eager $ numericBinOp div),
             ("><", eager $ strBinOp (++)),
             ("==", eager $ boolBinOp (==)), ("!=", eager $ boolBinOp (/=)),
             ("=", defineVar)]

precedenceOf :: String -> Int
precedenceOf = fromMaybe 0 . (`lookup` operatorPrecedence)

operatorPrecedences = [10,9..0]
operatorPrecedence = [("+", 5), ("-", 5),
                      ("*", 4), ("/", 4), ("><", 6),
                      ("==", 8), ("!=", 8)]

operate :: String -> Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue
operate op env left right =
  maybe (liftThrows $ throwError $ BadOp op) 
        (\ fn -> fn env left right) 
        (lookup op operators)

numericBinOp :: (Int -> Int -> Int) -> (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
numericBinOp op _ (Number l) (Number r) = liftThrows $ return $ Number $ op l r
numericBinOp op env (String str) r = numericBinOp op env (Number (read str)) r
numericBinOp op env l (String str) = numericBinOp op env l $ Number $ read str
numericBinOp op _ l (Number _) = liftThrows $ throwError $ TypeMismatch "Number" (show l)
numericBinOp op _ (Number _) r = liftThrows $ throwError $ TypeMismatch "Number" (show r)

strBinOp :: (String -> String -> String) -> (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
strBinOp op _ left right = liftThrows $ return $ String $ op (show left) (show right)

boolBinOp :: (String -> String -> Bool) -> (Env -> TPLValue -> TPLValue -> IOThrowsError TPLValue)
boolBinOp op _ left right = liftThrows $ return $ Boolean $ op (show left) (show right)

unpack :: ThrowsError TPLValue -> TPLValue
unpack (Right val) = val
unpack (Left err) = String $ show err

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: Env -> String -> IO String
evalString env exp = runIOThrows $ liftM show $ 
                     (liftThrows $ readExp exp) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env exp = evalString env exp >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = 
  do result <- prompt
     if pred result 
       then return ()
       else action result >> until_ pred prompt action

main :: IO ()
main = nullEnv >>= until_ (== "quit") (readPrompt "~>") . evalAndPrint