module TPL.Parse where

import Control.Applicative           ((<$>), (<$), (<*), (*>), (<*>))
import Text.ParserCombinators.Parsec

import TPL.Value
                                             
comment :: Parser ()
comment = () <$ (string "--" *> many (noneOf "\n"))

whitespace :: Parser ()
whitespace = skipMany (() <$ oneOf " \t" <|> comment)

allSpaces :: Parser ()
allSpaces = skipMany (() <$ oneOf " \t" <|> comment <|> () <$ space)

idChar :: Parser Char
idChar = letter <|> digit <|> char '_'

idStartChar :: Parser Char
idStartChar = letter <|> char '_'
          
keyword :: String -> Parser String
keyword str = try (string str) <* notFollowedBy idChar <* whitespace <?> str

stringLiteral :: Parser Term
stringLiteral = StringLiteral <$> (strLit '\'' <|> strLit '"') <* whitespace <?> "string"
  where strLit quote = char quote *> contents quote <* char quote
        contents quote = many $ (char '\\' *> specChar) <|> noneOf [quote] 
        specChar = spec <$> escapeCharacter
        escapeCharacter = oneOf "\"\\nt'" <?> "valid escape character (\", n, t, \\, or ')"
        spec character = case character of
          'n' -> '\n'
          't' -> '\t'
          c   -> c

bool :: Parser Term
bool = BoolLiteral . (== "true") <$> (keyword "true" <|> keyword "false") <* whitespace <?> "boolean"

num :: Parser Term
num = NumericLiteral . read <$> many1 digit <* whitespace <?> "number"

nullExp :: Parser Term
nullExp = NullLiteral <$ keyword "null"

name :: Parser String
name = (:) <$> idStartChar <*> many idChar

identifier :: Parser Term
identifier = Id <$> name <* whitespace <?> "identifier"

operator :: Parser Term
operator = Operator <$> (op <|> char '`' *> name <* char '`') <* whitespace
  where opChars = "+-=*&^%#@!?/.|~<>:" -- TODO: Systematically identify valid opChars...
        op = many1 $ oneOf opChars
        
list :: Parser Term
list = ListLiteral <$> between (char '[' *> allSpaces) (char ']' *> whitespace) contents
  where contents = expression `sepBy` char ',' <* allSpaces

lambda :: Parser Term
lambda = oneOf "\\λ" *> (Lambda <$> parameters <*> body)
  where parameters = allSpaces *> many argument
        argument = identifier <|> list <|> delayedExp
        body = string "->" *> allSpaces *> expression
        
delayedExp :: Parser Term
delayedExp = char '$' *> allSpaces *> (Lambda [] <$> atom) 

block :: Parser Term
block = between (char '(' *> allSpaces) (char ')' *> allSpaces) expressions

expression :: Parser Term
expression = Expression <$> many1 atom <?> "expression" 

atom :: Parser Term
atom =  lambda
    <|> bool
    <|> nullExp
    <|> identifier
    <|> stringLiteral
    <|> num
    <|> operator
    <|> list
    <|> delayedExp
    <|> block
    
expressions :: Parser Term
expressions = allSpaces *> (Block <$> many (expression <* end))
  where terminator = () <$ oneOf ";\n" <|> eof
        end = (terminator *> allSpaces) <|> lookAhead (() <$ char ')')