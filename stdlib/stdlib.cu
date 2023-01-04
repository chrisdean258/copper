fn print(*args) write(1, *args, '\n')
fn prints(*args) write(1, *args)

class range {
	field current, stop, stride

	fn __init__(self, start, stop=null, stride=1) {
		self.stride = stride
		if self.stop <- stop {
			self.current = start
		}
		else {
			self.current = 0
			self.stop = start
		}
	}

	fn __iter__(self) {
		val = self.current
		if (self.stride > 0 && val >= self.stop) || (self.stride < 0 && val <= self.stop)
			null
		else {
			self.current += self.stride
			val
		}
	}
}
