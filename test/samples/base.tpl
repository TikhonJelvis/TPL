print (precedenceOf (+))
print (precedenceOf (*))

a ~ b := a + b
precedence (~) 10
print (precedenceOf (~))

print (precedenceOf "*application*")

print (flip (-) 1 2)
-- TODO: test unary flip!

print (trimPrecs (exprToString $(+)))

super := {x : 10}
sub   := {y : 11} :> super

print (super.x)
print (sub.y)
print (sub.x)

super.y := 100
print (super.y)
print (sub.y)

super.z := 42
print (super.z)
print (sub.z)

super.y <- 0
print (super.y)
print (sub.y)

super.z <- 123
print (super.z)
print (sub.z)

-- TODO: Figure out how to test missing variables and the like!

print (if true 1 else (print "fail"))
print (if false (print "fail") else 1)

print (force $10)