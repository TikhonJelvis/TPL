true  ==> print "pass"
false ==> print "fail"

true  ? print "pass" # print "fail"
false ? print "fail" # print "pass"

else ? print "pass" # print "fail"

switch 10 [
    11          => puts "fail",
    10          => puts "pass",
    10          => puts "fail",
    puts "fail" => null
]

cond [
    false        => print "fail",
    true         => print "pass",
    print "fail" => null
]

typecase 10 [
    "number" => print "pass",
    "string" => print "fail"
]

let {x : "pass"} (
    print x
)

let [["x", "pass"]] (
    print x
)

for x in (1..10) (
    print x
)

i := 0
while (i < 10) (
    i <- i + 1
    print i
)

i <- 0
do (
    i <- i + 1
    print i
) while (i < 10)
do (
    i <- i + 1
    print i
) while (i < 10)
