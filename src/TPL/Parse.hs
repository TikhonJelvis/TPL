module TPL.Parse (TPLValue (..), expressions, parse, operatorCharacters) where

import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import TPL.Value
                                             
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
keyWord str = lexeme $ do str <- try $ string str
                          notFollowedBy idChar
                          return str

specChar :: CharParser st Char
specChar = spec <$> (oneOf "\"\\nt'"
                     <?> "valid escape character (\", n, t, \\, or ')")
  where spec char = case char of
          'n'  -> '\n'
          't'  -> '\t'
          c    ->  c -- All other characters are just themselves when escaped.
    
stringLiteral :: Parser TPLValue
stringLiteral = do opener   <- oneOf "\"'"
                   contents <- many $ (char '\\' >> specChar) <|> noneOf [opener]
                   char opener
                   return $ String contents
bool :: Parser TPLValue
bool = Boolean . (== "true") <$> (keyWord "true" <|> keyWord "false")

number :: Parser TPLValue
number = Number . read <$> many1 digit <?> "number"

nullExp :: Parser TPLValue
nullExp = keyWord "null" >> return Null
               
identifier :: Parser TPLValue
identifier = do head     <- letter <|> char '_'
                contents <- many idChar
                return . Id $ head:contents

operatorCharacters = "+-=*&^%#@!?/.|~<>:"
operator :: Parser TPLValue
operator = Operator <$> many1 (oneOf operatorCharacters) 

list :: Parser TPLValue
list = List <$> between (lexeme $ char '[') (char ']') (expression `sepBy` lexeme (char ','))

lambda :: Parser TPLValue
lambda = do oneOf "\\λ"
            parameters <- parameterList
            lexeme $ string "->"
            body       <- expression
            return $ Lambda parameters body
  where parameterList = whiteSpace >> many (lexeme argument)
        argument      = identifier <|> list

expression :: Parser TPLValue
expression = Expression <$> many1 atom

block :: Parser TPLValue
block = Sequence <$> between (lexeme $ char '{') (char '}') (expression `sepEndBy` lexeme terminator)

ifStatement :: Parser TPLValue
ifStatement = try $ do keyWord "if"
                       condition <- lexeme parenExp
                       (consequent, alternate) <- try blockElse <|> try expElse <|> ifOptBlock
                       return $ If condition consequent alternate
  where ifOptBlock = do body <- expression
                        return (body, Null)
        blockElse  = do consequent <- block
                        keyWord "else"
                        alternate <- expression
                        return (consequent, alternate)
        expElse    = do consequent <- lexeme $ manyTill atom (try $ string "else")
                        alternate <- expression
                        return (Expression consequent, alternate)
        

parenExp :: Parser TPLValue
parenExp = between (lexeme $ char '(') (char ')') $ expression

delayedExp :: Parser TPLValue
delayedExp = do char '$'
                Lambda [] <$> atom

atom :: Parser TPLValue
atom = lexeme $ lambda
            <|> ifStatement
            <|> bool
            <|> nullExp
            <|> identifier
            <|> stringLiteral
            <|> number
            <|> operator
            <|> list
            <|> parenExp
            <|> block
            <|> delayedExp

expressions :: Parser TPLValue
expressions = Sequence <$> expression `sepEndBy` lexeme terminator
