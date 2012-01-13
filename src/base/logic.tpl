load 'base/list';

and := fold1 (&);
or  := fold1 (|);

majority ls := {
  [t, f] := fold (\ [t, f] b -> if (b) [t + 1, f] else [t, f + 1]) [0, 0] ls;
  t > f;
};

none ls := filter id ls = [];
