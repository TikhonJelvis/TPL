not b := if b false else true

a | $b := if a a else b
precedence (|) 3

a & $b := if a b else a
precedence (&) 3

a /= b := not (a = b)
precedence (/=) (precedenceOf (=))

is x := x /= null
-- TODO: maybe implement majority? other logic functions?