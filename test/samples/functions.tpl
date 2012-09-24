f := \ x -> x
print (f 42)
print (f f f f f f f f f f f 42)

f x := x
print (f 42)
print (f f f f f f f f f f f 42)

f a b c := a + b * c
print (f 1 2 3)

a `f` b := a + b
print (f 1 2)
print (1 `f` 2)

print (typeof f)
print (typeof (f 1))

y := 0
f x := y <- x
print y
f 10
print y
f 42
print y

z := 0
f x := (
    z <- z + 1
    z + x
)
print z
print (f 1)
print (f 1)
print (f 2)

f $x := 42
f (print "fail")

f x $y := x
print (f 42 (print "fail"))

f [x, xs...] := x
print (f [])
print (f [1])
print (f [1,2,3])

f [x, xs...] := xs
print (f [])
print (f [1])
print (f [1,2,3])

f [[a, b], [as, bs]...] := a
print (f [[1, 2], [3, 4]])

f [[a, b], [as, bs]...] := as
print (f [[1,2], [3,4], [5,6], [7,8], [9,10], [11,12]])
