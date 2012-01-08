load 'base/list';

not a := if (a) false else true;

and := fold1 (&);
or  := fold1 (|);

majority ls := {
  [t, f] := fold (\ [t, f] b -> if (b) [t + 1, f] else [t, f + 1]) [0, 0] ls;
  t > f;
}
