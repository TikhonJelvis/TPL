require 'base/list'

condition ==> $result := if condition result else false
precedence (==>) 4

pred ? $then := \ $alt -> if pred then else alt
precedence (?) 3

else := true

$a => $b := [$a, $b]
precedence (=>) 1
switch value [[condition, res], rest...] := value = condition ? res # is rest ==> switch value rest
cond [[condition, res], rest...] := is condition ==> (condition ? res # cond rest)

typecase x cases := switch (typeof x) cases

let bindings $body := typecase bindings [
    "object" => (
        bindings :> get "*context*"
        force (with bindings body)
    ),
    "list" => (
        env := {} :> get "*context*"
        map (λ [name, value] -> defineObj env name value) bindings
        force (with env body)
    )
]

for $x $in ls $body := (
    map (λ item -> force (with ({x : item} :> get "*context*") body)) ls
    null
)