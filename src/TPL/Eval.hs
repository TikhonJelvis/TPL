module TPL.Eval where

import Text.ParserCombinators.Parsec (parse)

import TPL.Error                     (Error(..))
import TPL.Parse                     (expressions)
import TPL.Value

readExpr :: String -> Either Error Term
readExpr expr = case parse expressions "TPL" expr of
  Left err  -> Left $ Parser err
  Right val -> Right val

eval :: Env -> Term -> IO Result
eval = undefined