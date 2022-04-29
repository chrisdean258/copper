fn range(start, stop=null, stride=null) {
	real_stride = if (stride == null) 1 else stride
	real_stop = if (stop == null) start else stop
	real_start = if (stop == null) 0 else start
	\if (real_start < real_stop) {
		val = real_start
		real_start += real_stride
		val
	}
}

fn int(chrs) {
	rv = 0
	for (chr in chrs) {
		ch = chr - '0' 
		rv = 10 * rv + ch
	}
	rv
}

fn map(l, f) {
	rv = []
	for (i in l) {
		rv += [f(i)]
	}
	rv
}

fn reduce(list, fun)  {
	rv = list[0]
	for (i in range(1, len(list))) {
		rv = fun(rv, list[i])
	}
	rv
}

fn fold(list, fun, val) {
	for (v in list) {
		val = fun(val, v)
	}
	val
}

fn filter(list, predicate) {
	rv = []
	for (val in list) {
		if (predicate(val)) {
			rv += [val]
		}
	}
	rv
}

fn sum(l) reduce(l, \0 + \1)

fn sort(list) {
	fn swap(l, a, b) {
		t = l[a]
		l[a] = l[b]
		l[b] = t
	}

	fn partition(list, low, high) {
		pivot = list[high]
		i = low - 1
		j = low
		while(j < high) {
			if(list[j] < pivot) {
				i++
				swap(list, i, j)
			}
			j++
		}
		swap(list, i+1, high)
		i + 1
	}

	fn qsort(list, low, high) {
		if (low < high) {
			pi = partition(list, low, high)
			qsort(list, low, pi - 1)
			qsort(list, pi + 1, high)
		}
	}
	qsort(list, 0, len(list) -1)
}

fn abs(a) if (a < 0) -a else a
fn min(a, b) if (a < b) a else b
fn max(a, b) if (a > b) a else b

fn str_split(input, sep) {
	fn streq(str, offset, haystack) {
		i = 0
		rv = true
		for (i in range(len(haystack))) {
			if (i + offset >= len(str) || haystack[i] != str[i + offset]) {
				rv = false
			}
		}
		rv
	}
	rv = []
	cur = []
	i = 0
	while(i < len(input)) {
		if (!streq(input, i, sep)) {
			cur += [input[i]]
			i++
		}
		else {
			rv += [cur]
			cur = []
			i += len(sep)
		}
	}
	rv += [cur]
	rv
}
