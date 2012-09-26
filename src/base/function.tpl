id x := x                       -- identity
const x $y := x                 -- the K combinator
ignore := const null            -- throw x away without evaluating

f @ g := \ x -> f (g x)         -- function composition--@ looks like ∘ if you squint :)
precedence (@) 8

f # $x := f x                   -- like Haskell's $ operator for function application
precedence (#) 2

-- This function is from Haskell's Data.Function. You can do stuff
-- like eqaul `on` fst, or something...
binary `on` unary := \ a b -> binary (unary a) (unary b)
