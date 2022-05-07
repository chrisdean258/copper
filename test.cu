# b = if true 1 and if false 2 else 3
# a = if true 1 and if true 2 else 3
# c = if false 1 and if true 2 else 3
# d = if false 1 and if false 2 else 3
fn a(b) b

v = if false a(69) else a(99)
