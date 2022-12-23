s = if true 1 else null
n = if false 1 else null

print(s || 2) # 1
print(n || 2) # 2
print(s || n) # 1
print(n || s) # 1
print(n && s) # null
print(s && n) # null
print(n ^^ s) # 1
print(s ^^ n) # 1
print(s ^^ s) # null
print(n ^^ n) # null



