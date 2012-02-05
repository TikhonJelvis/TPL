module TPL.Parse (TPLValue (..), expressions, parse, operatorCharacters) where

import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import TPL.Value
                                             
whiteSpace :: CharParser st ()
whiteSpace = skipMany $ oneOf " \t"

terminator :: CharParser st ()
terminator = (oneOf ";\n" >> return ()) <|> eof

lexeme :: CharParser st a -> CharParser st a
lexeme p = do res <- p
              whiteSpace
              return res
              
wLexeme :: CharParser st a -> CharParser st a
wLexeme p = do res <- p
               spaces
               return res

idChar :: Parser Char
idChar = letter <|> digit <|> oneOf "_"

keyWord :: String -> Parser String
keyWord str = lexeme $ do res <- try $ string str
                          notFollowedBy idChar
                          return res

specChar :: CharParser st Char
specChar = spec <$> (oneOf "\"\\nt'"
                     <?> "valid escape character (\", n, t, \\, or ')")
  where spec character = case character of
          'n'  -> '\n'
          't'  -> '\t'
          c    ->  c -- All other characters are just themselves when escaped.
    
stringLiteral :: Parser TPLValue
stringLiteral = do opener   <- oneOf "\"'"
                   contents <- many $ (char '\\' >> specChar) <|> noneOf [opener]
                   char opener <?> "end of string"
                   return $ String contents
bool :: Parser TPLValue
bool = Boolean . (== "true") <$> (keyWord "true" <|> keyWord "false")

number :: Parser TPLValue
number = Number . read <$> many1 digit <?> "number"

nullExp :: Parser TPLValue
nullExp = keyWord "null" >> return Null
               
identifier :: Parser TPLValue
identifier = do first     <- letter <|> char '_'
                contents <- many idChar
                return . Id $ first:contents

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
  where parameterList = whiteSpace >> many (lexeme argument)
        argument      = identifier <|> list

expression :: Parser TPLValue
expression = Expression <$> many1 atom <?> "expression"

terminatedExpression :: Parser TPLValue
terminatedExpression = do result <- expression
                          try (wLexeme terminator) <|> lookAhead (char '}' >> return ())
                          return result

-- TODO: Rewrite the expression `sepBy` parser to end with a normal expression!
block :: Parser TPLValue
block = Sequence <$> between (wLexeme $ char '{') (char '}') code
  where code = many terminatedExpression

parenExp :: Parser TPLValue
parenExp = between (wLexeme $ char '(') (char ')') $ expression

delayedExp :: Parser TPLValue
delayedExp = do char '$'
                Lambda [] <$> atom

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
