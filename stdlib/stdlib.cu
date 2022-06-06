# fn print(a1="", a2="", a3="", a4="", a5="", a6="", a7="", a8="") {
	# write(a1, a2, a3, a4, a5, a6, a7, a8, '\n')
# }

fn print(*args) write(1, *args, '\n')
fn prints(*args) write(1, *args)

# fn print(a1="", a2="", a3="", a4="", a5="") {
	# write(1, a1, a2, a3, a4, a5, '\n')
# }

# fn prints(a1="", a2="", a3="", a4="") {
	# write(1, a1, a2, a3, a4)
# }
