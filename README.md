# Copper

This is a toy programming language that I am creating to learn rust and
programming language ideas.

Its kind of like a more thought out javascript (jk)

Its got pretty standard operations and some custom syntaxes

`fn fname(args..) body` for functions

We can declare a lambda function with the `\` operator and reference arguments with `\0`, `\1`,...

You can just use a lambda argument and it will start a lambda if you havent already

for example
```copper
fn do_action_if(val, cond, action) if cond(val) action(val)

do_action_if(10, \0 % 2 == 0, \print(\0))
```

will print 10

Its also worth noting that you dont need semicolons, newlines, or even spaces
between expressions or statements, copper will figure out what you meant.

Braced expressions will return the last value calculated in the expression.

```copper
fn do_a_thing() {
    print("hi")
    1
}
```
returns 1

```copper
fn if_false() if false 5 else 8
```
returns 8

### Flow control

Copper uses pretty standard control flows. 

#### If Expressions

```copper
if true print(1) else print(2)
```

will print `1`

but also 

```copper
print(if true 1 else 2)
```

will print `1`

### While Loops

```copper
i = 1
while true i++
```
will loop infinitely
```copper
i = 0
while true {
	i += 1
	if i > 9 { break }
}

print(i)
```

will print 10 as the look ends with the `break`. `continue` works similarly to c as well.
Note: `break` and `continue` must be used within brackets or followed by a
semicolon but this restrictiopn may be lifted in the future

#### For Loops

For loops work over lists and classes. Details of class iteration are explained with the class explanation

```copper
a = [1,2,3]
for i in a print(i)
```
prints
```
1
2
3
```

### Types

Currently only some basic types are supported:
- int
- float
- char
- string
- T? (Optional&lt;T&gt;)
- \[T\] (Vec&lt;T&gt;)
- Classes

#### Strings

Strings are immutable and indexable

#### Option Types

You can create an optional by conditionally returning a value or null from an
if statment You can compare and optional to `null` or to a value of its
underlying type You can extract the value with the unary `*` operator

```copper
a = if true 1 else null

if a == null print("a was null") else print(*a)

```

Additionally a variable assigned `null` will be promoted to an T? (read Option T) if assigned another value

```copper
a = null
a = 1 # a is now type `int?`
```

It is worth noting that a comparison to null with ONLY compare the outermost
level to null. I.E

```copper
i = if true null else 1
j = if false null else i
print(j == null)
```
prints `false` and

```copper
i = if true null else 1
j = if true null else i
print(j == null)
```
prints `true` and 
```copper
i = if true null else 1
j = if false null else i
print(*j == null)
```
prints `true`

as the type of null is deduced based on the expression to the 'most' optional value

#### Lists

It _should_ go without saying that lists are 0-indexed (looking at you lua).

```copper
a = [1, 2, 3]
print(a[1])
```
prints `2`

They support `+=` to add elements

```copper
a = [1, 2, 3]
a += [4]
print(a[3])
```
prints `4`

Empty lists will promotes into fully typed lists

```copper
a = []
a += [1] # now a is of type [int]
```

Options and Lists are currently not part of the class system but should be
integrated at some time in the future

#### Classes

```copper
class foo {
    field a, b, c
    fn __init__(self, a, b, c) {
        self.a = a
        self.b = b
        self.c = c
    }

    fn method(self) {
        return self.a + self.b
    }
}

bar = foo(1, 2, 3)
baz = bar.method()
```

Classes are defined as such. The `foo()` call will allocate and call the
`__init__` method with the passed in args. Methods are called with standard
syntax. The self parameter refers to the object instance. It is only convention
to call it self.

Note: the `__init__` name is subject to change

##### Class iteration

For class iteration two methods are relevant, `__iter__` and `__next__`

The `__iter__` method should return an iterator object over the values of the object

The iterator object's `__next__` method return items which should be iterated over
If it is a finite set of items the method should return a T? and a null value
will stop iteration

For example see tests 61-65


### Var args

Copper supports variable arguments to functions. Currently the only thing that
can be done with these is pass them along to a builtin function but hopefully
soon they will be iterable

```copper
fn print(*args) write(1, *args, '\n')
```

This is the current definition of print in the stdlib. It passes all its
arguments to `write`, a builtin, and sets the file decriptor as 1 and prints a
newline

### Builtins

The current builtins are `write`, `alloc`, and `len` however these functions
should likely not be used yet except for `write`

## To Do

### Slightly Less near future

- Moving to a span based system for locating tokens/expressions

### Aspirational

- Stdlib classes such as hashmap/dicts
- Enums / Algebraic Type system
- JSON support
