l = [1]
count = 0
for (i in range(100000)) {
	count += 1
	l = [i + 0]
	l[0] = 1342
	l += [1]
	l = [1]
}

print(count)

