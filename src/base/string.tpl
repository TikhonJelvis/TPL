load 'base/control';

toChars str := if (str = "") []
             else substr str 0 1 : (toChars @ substr str 1 (length str));

str % vals := {
    process [c1, c2, chars...] [val, vals...] :=
        if (c1 = '%' & c2) switch c2 [
            '%' -> {vals <- val:vals; '%'},
            's' -> val,
            'd' -> val * 1
        ] >< (chars & process chars vals | '')
      else c1 >< (c2 & process (c2:chars) (val:vals) | '');
    process (toChars str) vals;
};
