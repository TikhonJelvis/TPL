require 'base/control'

empty str := str = ""
toChars str := empty str ? [] # substr str 0 1 : toChars (substr str 1 # strlen str)

str % vals := (
    process [c1, c2, chars...] [val, vals...] := if (c1 = '%' & is c2) (
        switch c2 [
            '%' => (vals <- val:vals; '%'),
            's' => val,
            'd' => toString val
        ] >< (is chars ? process chars vals # "")
    ) else (
        is c1 ? c1 >< (is c2 ? process (c2:chars) (val:vals) # "") # ""
    )
    process (toChars str) vals
)