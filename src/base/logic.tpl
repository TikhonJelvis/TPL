a | $b := if (a) a else b;
a & $b := if (not a) false else b;

majority ls := {
  [t, f] := fold (\ [t, f] b -> if (b) [t + 1, f] else [t, f + 1]) [0, 0] ls;
  t > f;
};
