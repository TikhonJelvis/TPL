module TPL.Parse (TPLValue (..), expressions) where

import Control.Applicative ((<$>))
import Data.List
import Text.ParserCombinators.Parsec

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
              | Native String
              | If TPLValue TPLValue TPLValue

showSeq vals = intercalate " " $ map show vals

instance Show TPLValue where
  show (Null)                 = "null"
  show (Id id)                = id
  show (String str)           = show str
  show (Number int)           = show int
  show (Operator name)        = name
  show (Boolean bool)         = if bool then "true" else "false"
  show (List vals)            = show vals
  show (Expression vals)      = showSeq vals
  show (Sequence vals)        = unlines $ map show vals
  show (Function params body) =
    "λ " ++ showSeq params ++ " → {" ++ show body ++ "}"
  show (If condition consequent alternate) = "{?if " ++ show condition ++
                                             " then " ++ show consequent ++
                                             " else " ++ show alternate ++ "?}"
                                             
whiteSpace :: CharParser st ()
whiteSpace = spaces

terminator :: CharParser st Char
terminator = oneOf ";"

lexeme :: CharParser st a -> CharParser st a
lexeme = (>>= \ res -> whiteSpace >> return res)

idChar = letter <|> digit <|> oneOf "_"

keyWord :: String -> CharParser st String
keyWord str = do str <- try . lexeme $ string str
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
               
identifier :: Parser TPLValue
identifier = do head     <- letter <|> char '_'
                contents <- many idChar
                return . Id $ head:contents

operator :: Parser TPLValue
operator = Operator <$> many1 (oneOf "+-=*&^%$#@!?/.|~<>:") 

list :: Parser TPLValue
list = List <$> between (lexeme $ char '[') (char ']') (expression `sepBy` lexeme (char ','))

lambda :: Parser TPLValue
lambda = do oneOf "\\λ"
            parameters <- parameterList
            lexeme $ string "->"
            body <- expression
            return $ Function parameters body
  where parameterList = whiteSpace >> many (lexeme identifier)

expression :: Parser TPLValue
expression = Expression <$> many1 atom

block :: Parser TPLValue
block = Sequence <$> between (char '{') (char '}') (expression `sepEndBy` lexeme terminator)

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
parenExp = between (char '(') (char ')') expression

atom :: Parser TPLValue
atom = lexeme $ lambda
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
expressions = Sequence <$> expression `sepEndBy` lexeme terminator
