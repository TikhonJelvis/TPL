not b := if (b) false else true
a | $b := with ['c' -> a] $(if c c else b)
a & $b := with ['c' -> a] $(if c b else c)

majority ls := {
  [t, f] := fold (\ [t, f] b -> if b [t + 1, f] [t, f + 1]) [0, 0] ls
  t > f
}
