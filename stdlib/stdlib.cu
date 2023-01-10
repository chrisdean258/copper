fn print(*args) write(1, *args, '\n')
fn prints(*args) write(1, *args)

fn range(start, stop=null, stride=1) {
	if !sstop <- stop {
		sstop = start
		start = 0
	}
	if stride >= 0 ForwardRange(start, stop, stride)
	else ReverseRange(start, stop, stride)

}

class ForwardRange {
	field current, stop, stride

	fn __init__(self, start, stop, stride) {
		self.current = start
		self.stop = stop
		self.stride = stride
	}

	fn __iter__(self) self

	fn __next__(self) {
		val = self.current
		if val >= self.stop
			null
		else {
			self.current += self.stride
			val
		}
	}
}

class ReverseRange {
	field current, stop, stride

	fn __init__(self, start, stop, stride) {
		self.current = start
		self.stop = stop
		self.stride = stride
	}

	fn __iter__(self) self

	fn __next__(self) {
		val = self.current
		if val <= self.stop
			null
		else {
			self.current += self.stride
			val
		}
	}
}

class count {
	field current, stride

	fn __init__(self, start=0, stride=1) {
		self.current = start
		self.stride = stride
	}

	fn __iter__(self) self

	fn __next__(self) self.current += self.stride
}

fn sum(vals) {
	acc = 0
	for v in vals acc += v
	acc
}

class ListFilter {
	field list, cond, idx
	fn __init__(self, list, cond) {
		self.list = list
		self.cond = cond
		self.idx = 0
	}

	fn __iter__(self) self

	fn __next__(self) {
		if self.idx < len(self.list) {
			self.list[self.idx++]
		} else null

	}
}


fn filter_list(vals, cond) {
	ListFilter(vals, cond)
}
