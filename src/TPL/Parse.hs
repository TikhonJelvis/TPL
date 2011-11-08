module TPL.Parse (TPLValue (..), expressions) where

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
