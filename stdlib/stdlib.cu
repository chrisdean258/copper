fn print(*args) write(1, *args, '\n')
fn prints(*args) write(1, *args)

class range {
	field current, stop, stride

	fn __init__(self, start, stop=null, stride=null) {
		if sstop <- stop {
			self.current = start
			self.stop = sstop
			self.stride = stride || 1
		}
		else {
			self.current = 0
			self.stop = start
			self.stride = 1
		}
	}

	fn __iter__(self) {
		val = self.current
		if val >= self.stop
			null
		else {
			self.current += self.stride
			val
		}
	}
}
