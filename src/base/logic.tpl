not b := if (b) false else true

a | $b := with ['c' -> a] $(if c c else b)
precedence (|)  8

a & $b := with ['c' -> a] $(if c b else c)
precedence (&)  8

majority ls := (
  [t, f] := fold (\ [t, f] b -> if b [t + 1, f] [t, f + 1]) [0, 0] ls
  t > f
)
