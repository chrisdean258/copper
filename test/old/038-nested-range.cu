fn range(stop) {
	i = 0
	\if(i < stop) i++
}

for (i in range(10)) for (j in range(10)) print(i, " ", j)
