force x := x

print x := _print x

precedence (+)  5
precedence (-)  5
precedence (*)  4
precedence (/)  4
precedence (=)  7
precedence (/)  7
precedence (:)  9
precedence (!)  6
precedence (:=) 11
precedence (<-) 11

if cond $then $_ $else := force (_if cond $then $else)

let $bindings $body := with bindings body

require 'base/control'
require 'base/list'
require 'base/function'
require 'base/logic'
require 'base/math'
require 'base/string'