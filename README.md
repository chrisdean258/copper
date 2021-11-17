# Copper

This is a toy programming language that I am creating to learn rust and
programming language ideas.

Its kind of like a more thought out javascript (jk)

Its got pretty standard operations and some custom syntaxes

`fn fname(args..) body` for functions

We can declare a lambda function with the `\` operator and reference arguments with `\0`, `\1`,...

You can just use a lambda argument and it will start a lambda if you havent already

for example
```
fn do_action_if(val, cond, action) if cond(val) action(val)

do_action_if(10, \0 % 2 == 0, print)
```

will print 10

Its also worth noting that you dont need semicolons or newlines, copper will
figure out what you meant.

Braced expressions will return the last value calculated in the expression.
Return statements are not yet implemented

```
fn do_a_thing() {
    print("hi")
    1
}
```
returns 1

```
fn if_false() if(false) 5 else 8
```
returns 8


### Types
Currently only some basic types are supported:
- Int
- Char
- String
- Null
- Nullable<T> where T is one of the above

A nullable value is created when on creation a variable is assigned a null value then later assigned another non null value

When it comes to types, you cannot change type except for a Null type promoting to a Nullable type

