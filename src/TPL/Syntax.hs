module TPL.Syntax where

import           Data.Functor ((<$>))

import           TPL.Value

-- TODO: Support left/right associative operators?
--       I currently only have left associative operators.
processOp :: String -> Term -> Term
processOp op = squash . deleteApps . handleOp . reifyApps . desugar
  where handleOp e@(Expression s expr)
          | Operator op `elem` expr = Expression s $ squash <$> [Id op, Expression "" left, Expression "" right]
          | otherwise               = e
          where right = reverse . takeWhile (/= Operator op) $ reverse expr
                left = reverse . drop 1 . dropWhile (/= Operator op) $ reverse expr
        handleOp notExpression = notExpression

        app = "*application*"
        notApp (Id x)       = x /= app
        notApp (Operator o) = o /= app
        notApp _            = True
        reifyApps (Expression s ls) = Expression s $ go ls
          where go (a:b:rest) | not $ isOp b || isOp a = a : Operator app : go (b : rest)
                              | otherwise           = a : go (b : rest)
                go a                                = a
        reifyApps x = x
        deleteApps (Expression s expr) = Expression s $ deleteApps <$> filter notApp expr
        deleteApps x                   = x

isOp :: Term -> Bool
isOp Operator{} = True
isOp _          = False

squash :: Term -> Term
squash (Expression _ [term]) = term
squash (Block [term])        = term
squash term                  = term


desugar :: Term -> Term
desugar (Expression s [Operator o, right])  = Expression s [Id "flip", Id o, right]
desugar (Expression s [left, Operator o])   = Expression s [Id o, left]
desugar (Expression _ [Operator o])         = Id o
desugar val                                 = val
