force x := x

precedence (+)  5
precedence (-)  5
precedence (*)  4
precedence (/)  4
precedence (>)  6
precedence (=)  7
precedence (/)  7
precedence (>)  7
precedence (<)  7
precedence (<=) 7
precedence (>=) 7
precedence (|)  8
precedence (&)  8
precedence (:)  9
precedence (!)  6
precedence (:=) 11
precedence (<-) 11

if cond $then $_ $else := force (_if cond $then $else)

let $bindings $body := with bindings body

require 'base/list'

