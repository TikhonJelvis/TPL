module TPL.Parse where

import           Control.Applicative           ((*>), (<$), (<$>), (<*), (<*>))

import           Data.Char                     (isPunctuation, isSymbol)
import           Data.List                     ((\\))

import           Text.ParserCombinators.Parsec

import           TPL.Value

  -- Parses an expression, keeping track of its source string.
expr :: Parser [Term] -> Parser Term
expr p = do inp  <- getInput
            res  <- p
            inp' <- getInput
            let len = length inp - length inp'
            return $ Expression (take len inp) res

comment :: Parser ()
comment = () <$ (try $ string "--" *> many (noneOf "\n")) <?> ""

whitespace :: Parser ()
whitespace = skipMany (() <$ oneOf " \t" <|> comment) <?> ""

allSpaces :: Parser ()
allSpaces = skipMany (() <$ oneOf " \t" <|> comment <|> () <$ space) <?> ""

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
  where op = many1 $ satisfy operatorSymbol
        operatorSymbol x = (isSymbol x || isPunctuation x) && (not $ x `elem` "$[]{}(),;`_")

list :: Parser Term
list = ListLiteral <$> between (char '[' *> allSpaces) (char ']' *> whitespace) contents
  where contents = (expression <* allSpaces) `sepBy` (char ',' <* allSpaces)

object :: Parser Term
object = char '{' *> allSpaces *> (ObjectLiteral <$> many binding) <* char '}' <* whitespace
  where binding = (,) <$> (key <* allSpaces <* char ':')
                      <*> (allSpaces *> expression <* end)
        key = try method <|> identifier <|> stringLiteral <|> num
        method = expr ((:) <$> identifier <*> many1 argument)

argument :: Parser Term
argument = identifier <|> list <|> delayedExp

lambda :: Parser Term
lambda = oneOf "\\λ" *> (Lambda <$> parameters <*> body)
  where parameters = allSpaces *> many argument
        body = (string "->" <|> string "→") *> allSpaces *> expression

delayedExp :: Parser Term
delayedExp = char '$' *> allSpaces *> (Lambda [] <$> atom)

block :: Parser Term
block = between (char '(' *> allSpaces) (char ')' *> whitespace) expressions

expression :: Parser Term
expression = expr (many1 atom) <?> "expression"

atom :: Parser Term
atom =  lambda
    <|> bool
    <|> nullExp
    <|> identifier
    <|> stringLiteral
    <|> num
    <|> operator
    <|> list
    <|> object
    <|> delayedExp
    <|> block

expressions :: Parser Term
expressions = allSpaces *> (Block <$> many (expression <* end))

program :: Parser Term
program = expressions <* eof

end :: Parser ()
end = (terminator *> allSpaces) <|> lookAhead (() <$ oneOf "})")
  where terminator = () <$ oneOf ";\n" <|> eof

