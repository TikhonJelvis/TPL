print (id 10)
print (id id id id id 10)

print (const 10 100)
print (const 10 (print "fail"))

ignore (print "fail")

f x := x >< "bar "
g x := x >< "baz"
print ((g @ f) "foo")
print ((f @ g) "foo")

f x := x
print (f # 1 + 2 * 3)

f x := if (x = 10) 1 else 10
print (((=) `on` f) 1 2)
print (((=) `on` f) 1 10)
print (((=) `on` f) 10 10)
