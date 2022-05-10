#!/usr/bin/env python3

def fib(a):
    if a < 2:
        return 1
    else:
        return fib(a - 1) + fib(a - 2)

print(fib(30))
