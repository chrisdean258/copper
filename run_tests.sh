#!/bin/bash


cargo build || exit 1

run-test()
{
	file="$1"
	if [ $# -gt 1 ]; then
		! diff -y <(./target/debug/copper "$file" 2>&1) "$file.out"
	else
		! diff -q <(./target/debug/copper "$file" 2>&1) "$file.out" &>/dev/null
	fi
}

color() {
	tput setaf "$1"
	shift
	echo "$@"
	tput sgr0
}





if [ $# -eq 0 ]; then
	num_tests=0
	num_passed=0

	for file in ./test/*.cu; do
		num_tests=$(($num_tests + 1))
		if run-test "$file"; then
			color 1 "Failed $file"
		else
			num_passed="$(echo "$num_passed" | awk '{print $1 + 1}')"
			color 2 "Passed $file"
		fi
	done
	echo "Passed $num_passed/$num_tests"
else 
	for t in "$@"; do 
		file="$(find ./test -name '*.cu' | grep "$t" | head -n 1)"
		run-test "$file" 1
	done
fi


