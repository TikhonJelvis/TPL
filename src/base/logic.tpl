a | $b := with ['c' -> a] $(if (c) c else b);
a & $b := if (a) b else false;

majority ls := {
  [t, f] := fold (\ [t, f] b -> if (b) [t + 1, f] else [t, f + 1]) [0, 0] ls;
  t > f;
};
