module Main where

import Control.Monad.Error
import Data.Maybe
import Text.ParserCombinators.Parsec

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
  show (String str) = str
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

data TPLEnv = Env (Maybe TPLEnv) [(String, TPLValue)]
nullEnv = Env Nothing []

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

readExpr :: String -> [TPLValue]
readExpr expr = case parse parseExpressions "TPL" expr of
  (Left err)   -> [String $ show err]
  (Right vals) -> vals

eval :: TPLValue -> ThrowsError TPLValue
eval (Expression [val]) = eval val
eval (Expression [a, (Operator op), b]) = do aVal <- eval a
                                             bVal <- eval b
                                             (operate op) aVal bVal
eval val = return val

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

operators = [("+", numericBinOp (+)), ("-", numericBinOp (-)),
             ("*", numericBinOp (*)), ("/", numericBinOp div),
             ("><", strBinOp (++)),
             ("==", boolBinOp (==)), ("!=", boolBinOp (/=))]

precedenceOf :: String -> Int
precedenceOf = fromMaybe 0 . (`lookup` operatorPrecedence)

operatorPrecedences = [10,9..0]
operatorPrecedence = [("+", 5), ("-", 5),
                      ("*", 4), ("/", 4), ("><", 4),
                      ("==", 8), ("!=", 8)]

operate :: String -> TPLValue -> TPLValue -> ThrowsError TPLValue
operate op left right = 
  maybe (throwError $ BadOp op) (\ fn -> fn left right) (lookup op operators)

numericBinOp :: (Int -> Int -> Int) -> (TPLValue -> TPLValue -> ThrowsError TPLValue)
numericBinOp op (Number l) (Number r) = return $ Number $ op l r
numericBinOp op (String str) r = numericBinOp op (Number (read str)) r
numericBinOp op l (String str) = numericBinOp op l $ Number $ read str
numericBinOp op l (Number _) = throwError $ TypeMismatch "Number" (show l)
numericBinOp op (Number _) r = throwError $ TypeMismatch "Number" (show r)

strBinOp :: (String -> String -> String) -> (TPLValue -> TPLValue -> ThrowsError TPLValue)
strBinOp op left right = return $ String $ op (show left) (show right)

boolBinOp :: (String -> String -> Bool) -> (TPLValue -> TPLValue -> ThrowsError TPLValue)
boolBinOp op left right = return $ Boolean $ op (show left) (show right)

unpack :: ThrowsError TPLValue -> TPLValue
unpack (Right val) = val
unpack (Left err) = String $ show err

main :: IO ()
main = getLine >>= putStr . unlines . (map toStr) . readExpr
  where toStr = show . unpack . (>>= eval) . handleInfix