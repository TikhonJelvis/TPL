module TPL.Parse (TPLValue (..), expressions, parse, operatorCharacters) where

import Control.Applicative ((<$>), (<$), (<*), (*>), liftA2)
import Text.ParserCombinators.Parsec
import TPL.Value
                                             
whiteSpace :: CharParser st ()
whiteSpace = skipMany $ oneOf " \t"

terminator :: CharParser st ()
terminator = () <$ oneOf ";\n" <|> eof

lexeme :: CharParser st a -> CharParser st a
lexeme p = p <* whiteSpace
              
wLexeme :: CharParser st a -> CharParser st a
wLexeme p = p <* spaces

idChar :: Parser Char
idChar = letter <|> digit <|> oneOf "_"

keyWord :: String -> Parser String
keyWord str = lexeme $ try (string str) <* notFollowedBy idChar

specChar :: CharParser st Char
specChar = spec <$> (oneOf "\"\\nt'" <?> "valid escape character (\", n, t, \\, or ')")
  where spec character = case character of
          'n'  -> '\n'
          't'  -> '\t'
          c    ->  c -- All other characters are just themselves when escaped.
    
stringLiteral :: Parser TPLValue
stringLiteral = do opener   <- oneOf "\"'"
                   contents <- many $ (char '\\' *> specChar) <|> noneOf [opener]
                   char opener <?> "end of string"
                   return $ String contents

bool :: Parser TPLValue
bool = Boolean . (== "true") <$> (keyWord "true" <|> keyWord "false")

number :: Parser TPLValue
number = Number . read <$> many1 digit <?> "number"

nullExp :: Parser TPLValue
nullExp = Null <$ keyWord "null"
               
identifier :: Parser TPLValue
identifier = Id <$> liftA2 (:) (letter <|> char '_') (many idChar)

operatorCharacters :: [Char]
operatorCharacters = "+-=*&^%#@!?/.|~<>:"

operator :: Parser TPLValue
operator = Operator <$> many1 (oneOf operatorCharacters) 

list :: Parser TPLValue
list = List <$> between (wLexeme $ char '[') (char ']') (wLexeme expression `sepBy` wLexeme (char ','))

lambda :: Parser TPLValue
lambda = do oneOf "\\λ"
            parameters <- parameterList
            lexeme $ string "->"
            body       <- expression
            return $ Lambda parameters body
  where parameterList = whiteSpace *> many (lexeme argument)
        argument      = identifier <|> list <|> delayedExp

expression :: Parser TPLValue
expression = Expression <$> many1 atom <?> "expression"

terminatedExpression :: Parser TPLValue
terminatedExpression = expression <* (wLexeme terminator <|> lookAhead (() <$ char '}'))

block :: Parser TPLValue
block = Sequence <$> between (wLexeme $ char '{') (char '}') (many terminatedExpression)

parenExp :: Parser TPLValue
parenExp = between (wLexeme $ char '(') (char ')') $ expression

delayedExp :: Parser TPLValue
delayedExp = char '$' *> (Lambda [] <$> atom)

atom :: Parser TPLValue
atom = lexeme $ lambda
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
expressions = Sequence <$> many terminatedExpression
