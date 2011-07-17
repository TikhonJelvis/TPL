module Main where

import Control.Monad.Error
import Data.Maybe
import Text.ParserCombinators.Parsec

data TPLValue = Id String
             | Number Int
             | String String
             | Operator String
             | List [TPLValue]
             | Expression [TPLValue]

showTPL :: TPLValue -> String
showTPL (Id id) = "<identifier> " ++ id
showTPL (String str) = "<string> " ++ str
showTPL (Number int) = "<integer> " ++ show int
showTPL (Operator name) = "<operator> " ++ name
showTPL (List vals) = "<list> [" ++ (show $ map show vals) ++ "]"
showTPL (Expression vals) = "<exp> (" ++ (show $ map show vals) ++ ")"
               
instance Show TPLValue where
  show = showTPL

data TPLError = Parser ParseError
              | BadOp String String
              | Default String

showTPLE :: TPLError -> String
showTPLE (Parser err) = show err
showTPLE (BadOp err op) = show err ++ " " ++ op
showTPLE (Default str) = str

instance Show TPLError where
  show = showTPLE
instance Error TPLError where
  noMsg = Default "An error has occured!"
  strMsg = Default

type ThrowsError = Either TPLError

specChar :: CharParser st Char
specChar = do char <- oneOf "\"\\nt"
              return $ case char of
                '"'  -> '"'
                'n'  -> '\n'
                't'  -> '\t'
                '\\' -> '\\'

parseString :: Parser TPLValue
parseString = do oneOf "\"'"
                 contents <- many $ (char '\\' >> specChar) <|> noneOf "\""
                 oneOf "\"'"
                 return $ String contents
                 
parseId :: Parser TPLValue                 
parseId = do head <- letter <|> char '_'
             contents <- many $ letter <|> digit <|> oneOf "_!?"
             return $ Id $ head:contents

parseOperator :: Parser TPLValue             
parseOperator = do op <- many1 $ oneOf "+-=*&^%$#@!?/.|~<>:"
                   return $ Operator op

parseNumber :: Parser TPLValue
parseNumber = do head <- digit <|> char '-'
                 tail <- many digit
                 return $ Number $ read $ head : tail
                 
parseList :: Parser TPLValue                 
parseList = do contents <- between (char '[') 
                                  (char ']') 
                                  (sepBy parseTPL (spaces >> char ','))
               return $ List contents
               
parseExpression :: Parser TPLValue
parseExpression = do vals <- many parseTPL
                     return $ Expression vals

parseParenExp :: Parser TPLValue
parseParenExp = between (char '(') (char ')') parseExpression

parseTPL :: Parser TPLValue
parseTPL = spaces >> (parseId
                  <|> parseString
                  <|> parseNumber
                  <|> parseOperator
                  <|> parseList
                  <|> parseParenExp)
           
parseExpressions :: Parser [TPLValue]
parseExpressions = do exps <- sepBy parseExpression (oneOf ";\n")
                      return exps

readExpr :: String -> [TPLValue]
readExpr expr = case parse parseExpressions "TPL" expr of
  (Left err)   -> [String $ show err]
  (Right vals) -> vals
  
handleInfix :: Int -> [TPLValue] -> TPLValue
handleInfix precedence (exp:right) = handleRem [] exp right
  where handleRem left exp [] = Expression $ left ++ [exp]
        handleRem left opr@(Operator op) right@(r:rs) = 
          if (precedenceOf op) == precedence 
          then Expression $ [Expression left, opr, handleInfix precedence right]
          else handleRem (left ++ [opr]) r rs

eval :: TPLValue -> ThrowsError TPLValue
eval num@(Number _) = return num
eval str@(String _) = return str
eval op@(Operator _) = return op
eval list@(List _) = return list
eval (Expression [a, (Operator op), b]) = do aVal <- (eval a)
                                             bVal <- (eval b)
                                             (operate op) aVal bVal

operators = [("+", numericBinOp (+)),
             ("-", numericBinOp (-)),
             ("*", numericBinOp (*)),
             ("/", numericBinOp div)]
            
operatorPrecedence = [("+", 6),            
                      ("-", 6),
                      ("*", 5),
                      ("/", 5)]

precedenceOf :: String -> Int
precedenceOf op = fromMaybe 0 (lookup op operatorPrecedence)

operate :: String -> TPLValue -> TPLValue -> ThrowsError TPLValue
operate op left right = maybe (throwError $ BadOp "Bad operator!" op)
                              (\ op -> op left right)
                              (lookup op operators)

numericBinOp :: (Int -> Int -> Int) -> (TPLValue -> TPLValue -> ThrowsError TPLValue)
numericBinOp op = \ (Number left) (Number right) -> return $ Number $ op left right

main :: IO ()
main = do line <- getLine
          putStrLn $ unlines $ map (show . eval) (readExpr line)
          return ()
