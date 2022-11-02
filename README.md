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

```copper
if true print(1) else print(2)
```

will print `1`

but also 

```copper
print(if true 1 else 2)
```

will print `1`


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
Note: `break` and `continue` must be used within brackets, but I am working on an expression form

`for` loops are yet to be implemented


### Types
Currently only some basic types are supported:
- Int
- Char
- String
- Optional&lt;T&gt; where T is one of the above
- List&lt;T&gt;

You can create an optional by conditionally returning a value or null from an
if statment You can compare and optional to `null` or to a value of its
underlying type You can extract the value with the unary `*` operator

```copper
a = if true 1 else null

if a == null print("a was null") else print(*a)

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

Lists are a work in progress but can be defined and indexed, although cannot be
changed in size. It _should_ go without saying that lists are 0-indexed
(looking at you lua).

```
a = [1, 2, 3]
print(a[1])
```
prints `2`

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

### Near Future

- Return values from eval
- Methods and method calls

### Slightly Less near future

- Lists 
- For loops
- Optional value interactions with `&&` and `||` operators
- Moving to a span based system for locating tokens/expressions

### Aspirational

- Stdlib classes such as hashmap/dicts
- Enums / Algebraic Type system
- JSON support
