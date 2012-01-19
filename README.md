TPL
===

## Example Code

Check out the `base` library for some example code. This comes in five files: `base.tpl`, `base/function.tpl`, `base/list.tpl`, `base/logic.tpl` and `base/math.tpl`.

This is just the standard library for the language. Right now it is *not* loaded by default; any serious code should have a `load 'base';` statement at the top. 

The library is mostly written in a very functional style. However, there are some interesting functions like `partitionBy` in `base/list.tpl` that are written in a more imperative style.

## Future Features:

In the near future, I want to add:
  
  - more extensive and useful pattern matching (not just destructuring assignment for lists).
  - better error handling and reporting
  - more standard functions (IO is particularly lacking right now)
  - laziness on demand--I want functions to be able to control which of their parameters gets evaluated
  - maybe an OOP system of some kind--most likely prototype-based (no classes)

## Details:

### Basics

`:=` lets you define variables and functions. Defining a variable is easy:

    x := 0;

You can define functions in two different ways:

    f := \ n -> n + 1;
    f n := n + 1;

Functions are called without parentheses; `f a b c` is equal to `f(a, b, c)` in C-like languages.

If you do not pass enough arguments to a function, it will just *curry* the arguments you did pass in. Given

    f a b c := a + b * c;

calling `f 1 2` results in:
   
    λ α1 → {λ a b c → {a + b * c} 1 2 α1}

This also means that functions with *no* arguments (often called "thunks") are called just by their name:

    f := \ -> 1 + 2;
    f = 3;

There is currently no way to reference a thunk without calling it. While I think this feature is not as necessary as it seems, I will probably add some way to do it in the future (maybe along with the "laziness-on-demand" stuff I want to add).

`\ a b c -> ...` is a lambda expression and can be used anywhere a normal expression can. You can also call a lambda directly:

    (\ x -> x + 10) 100 = 110;

The `=` tests for equality; any time I have a statement like this by itself in the docs, it means the statement is true.

`:=` will create a new variable in the current scope. The only way to create a new scope is in a function. 

Later on, you can change the value of `x` using `<-`. 

    x <- x + 1;

This will change the current binding of `x` no matter which scope you are currently in. If you use `:=` instead, it will create a new variable unless it already exists in the *current* scope.

    x := 0;
    (\ n -> x := n) 10;
    x = 0;
    (\ n -> x <- n) 10;
    x = 10;

All expressions have values; practically anything can be an expression. Particularly, a block of code (between { and }) is an expression with the value of its last expression; it can be put anywhere a normal expression could be. So:

    blarg := \ n -> if ({a := n; n <- n + 1; a + n} = 101) "blarg" else "not blarg";

is valid and `blarg 50` will return the string `blarg`.

### Delaying Expressions

You can "delay" expressions using a `$` prefix. Basically, delaying just pushes back when an expression is evaluated. Instead of being evaluated *immediately*, it is evaluated when it is next used. For example:

   x := 0;
   r := $x;
   r = 0;
   x <- 10;
   r = 10;
   x <- x + 1;
   r = 11;

You can similarly delay more complex expressions by wrapping them in parentheses or a block:

    x := 0;
    r := $(x + 10)
    x <- 10;
    r = 20;
    r <- ${a := 3; x + a};
    x <- 15;
    r = 18;

This gives you a bit more control over when things are evaluated and lets you use a simple form of reactive programming. For example, given a hypothetical function that returned the current mouse position, you could make a variable that is *always* 10px over from the mouse:

    nearX := $mouseX

### Operators

You can define custom operators. For example, `<<` is equal to `map` in the standard library:

    ls << fn := map fn ls;

You can also use "operator slices" like in Haskell:
   
    load 'base';
    map (+ 2) (1..10);

This code will add 2 to each of the list's items. The `+ 2` expression actually evaluates to a function in the form `λ α → α + 2`; the greek letter α is used because it cannot currently be used as a normal identifier. The opposite expression would have worked too: `2 +` results in `λ α -> 2 + α`. I suspect bad things might happen if you try to nest slices, so don't.

You can set the precedence of an operator--whether or not you've defined it--using the built-in `precedence` function:

    1 + 2 * 3 = 7;
    precedence (+) 3;
    1 + 2 * 3 = 9;

The precedence should be a number between 1 and 11; 10 is the default.

You can get the current precedence of an operator using `precedenceOf`.

    precedenceOf (*) = 4

Note how you need to wrap the operators in parentheses in these cases. Without the parentheses, the expressions will not be parsed correctly.

### Pattern Matching (Destructuring Assignment)

There is *very basic* pattern matching, which is really like "destructuring assignment" from JavaScript. I want to have proper pattern matching, but that comes later (if at all :)).

    [a, b] := [37, 42];
    [a, b] <- (1..10);
    [c, d] := 1;

Note how the two size-mismatched cases are both valid. In `[a, b] <- (1..10)`, `a` is `1` and b is `2`; the rest is thrown away. In `[c, d] := 1`, `c` is 1 and `d` is not defined.

Finally, you can also nest these patterns:

    [a, [b, c], d] := [1, [2, 3], 4];

You can match the rest of the list using `...`:

    [x, xs...] := (1..10);

Here `x = 1` and `xs = [2,3,4,5,6,7,8,9,10]`.

The `...` also works on nested patterns:

    [[a, b], [c, d]...] := zip (1..10) (11..20);

Here `a = 1`, `b = 11`, `c = 2..10` and `d = 12..10`.

You can also use these patterns in functions:

    f [a, b] := a + b;
    f [1, 2];

You cannot have multiple declarations of a function with different patterns yet. This is part of the "proper" pattern matching I want to add in the future. Maybe.

You can use these patterns when declaring an operator:

    [a, b] ~ [c, d] := a * c + b * d;

### Files and IO

You can also load other files. This will run those files in your current environment (so if you load a file inside a function, things from that file will only be available in *that* function!).

    load 'base';

You do *not* include the .tpl extension when loading files. `base.tpl` is the standard library and contains things like `map` that don't need to be native.

Since `load` is just a (native) function, you can pass it around like normal:

    map load ['base', 'example', 'blarg', 'stuff '];

You can also get the contents of a file as a string with `open`. This *does* need the extension:

    str := open 'blarg.txt';

You can also print to STDOUT with `print`:

    print "blarg";
    print 5;
    print (+);

This should work on *all* values including functions.
