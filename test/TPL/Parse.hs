module TPL.Parse.Test where

import Control.Applicative
import Control.Monad

import Data.List

import Test.QuickCheck

import TPL.Parse
import TPL.Value
  
permutationsOf :: [a] -> Gen [a]
permutationsOf = return . take 10 <=< listOf1 . oneof . map return

wrap :: String -> String -> String -> String
wrap start end str = start ++ str ++ end

spaces :: Gen String
spaces = take 3 <$> permutationsOf "\n\r\t "

sepBy :: Gen String -> Gen String -> Gen String
sepBy sep gen = listOf1 gen >>= \ ls -> foldM combine (head ls) $ take 10 ls
  where combine a b = do separator <- sep
                         return $ a ++ separator ++ b

-- Generators for strings representing each type:
nullStr      = return "null"
idStr        = do start <- oneof . map return $  idChar
                  rest  <- permutationsOf $ idChar ++ ['0'..'9']
                  return $ start : rest
  where idChar = '_' : ['a'..'z'] ++ ['A'..'Z']
numberStr    = permutationsOf ['0'..'9']
stringStr    = wrap "\"" "\"" <$> (escape <$> arbitrary)
  where escape str = str >>= \ c -> if c == '"' || c == '\\' then '\\':[c] else [c]
booleanStr   = oneof [return "true", return "false"]
operatorStr  = permutationsOf operatorCharacters
exprStr      = liftM2 (++) spaces $ sepBy spaces atomStr
parenExprStr = wrap "(" ")" <$> exprStr
listStr      = do contents <- sepBy (return ",") exprStr
                  return $ wrap "[" "]" contents
blockStr     = do contents <- sepBy (return ";") exprStr
                  return $ wrap "{" "}" contents
lambdaStr    = do sp1  <- spaces
                  sp2  <- spaces
                  args <- sepBy spaces $ oneof [idStr, listStr]
                  body <- exprStr
                  return $ "\\" ++ sp1 ++ args ++ "->" ++ sp2 ++ body
ifStr        = do sp1        <- spaces
                  sp2        <- spaces
                  cond       <- exprStr
                  consequent <- exprStr
                  return $ "if" ++ sp1 ++ "(" ++ cond ++ ")" ++ sp2 ++ consequent
ifElseStr    = do sp1       <- spaces
                  sp2       <- spaces
                  ifPart    <- ifStr
                  alternate <- exprStr
                  return $ ifPart ++ sp1 ++ "else" ++ sp2 ++ alternate
                  
atomStr      = do atom <- frequency [(299, oneof [booleanStr,
                                 nullStr, idStr, stringStr, numberStr,
                                 operatorStr]),
                                     (1, oneof [lambdaStr, ifStr, ifElseStr,
                                         parenExprStr, blockStr])]
                  end  <- spaces
                  return $ atom ++ end
                  
-- Some convenience functions:
                  
trim = unwords . words

isId (Id id) exp = id == trim exp
isId _ _         = False

isNumber (Number n) exp = read exp == n
isNumber _ _            = False

isString (String str) exp = read exp == str
isString _ _              = False

isBoolean (Boolean b) "true"  = b
isBoolean (Boolean b) "false" = not b
isBoolean _ _                 = False

isOperator (Operator op) val = op == trim val
isOperator _ _               = False

isBlock (Sequence _) _ = True
isBlock _ _            = False

isIf (If _ _ _) _ = True
isIf _ _          = False

isValid (Left _)  = False
isValid (Right _) = True

extract (Right (Sequence [(Expression [val])])) = val
extract (Right (Sequence [val]))                = val
extract (Right (Expression [val]))              = val
extract (Right val)                             = val

testParses inp test = forAll inp $ \ exp ->
  let res = parse expressions "TPL" exp in
  isValid res && test (extract res) exp

-- Test that parsing single values works:
prop_parseNull   = testParses nullStr $ \ n _ -> n == Null
prop_parseId     = testParses idStr isId
prop_parseNum    = testParses numberStr isNumber
prop_parseStr    = testParses stringStr isString
prop_parseBool   = testParses booleanStr isBoolean
prop_parseOp     = testParses operatorStr isOperator
prop_parseBlock  = testParses blockStr isBlock
prop_parseIf     = testParses ifStr isIf
prop_parseIfElse = testParses ifElseStr isIf