require 'base/list'

succ n := n + 1
pred n := n - 1

a ^ b := fold1 (*) # repeat a b

sum ls := fold1 (+) ls
product ls := fold1 (*) ls
