require 'base/list'

not b := if (b) false else true

a /= b := not (a = b)
precedence (/=) (precedenceOf (=))

is x := x /= null

a | $b := if (a /= false) a else b
precedence (|) 8

a & $b := if (a /= false) b else a
precedence (&) 8

majority ls := (
  [t, f] := fold (\ [t, f] b -> if b [t + 1, f] [t, f + 1]) [0, 0] ls
  t > f
)
