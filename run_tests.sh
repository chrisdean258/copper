#!/bin/bash


cargo build

run-test()
{
	file="$1"
	if [ $# -gt 1 ]; then
		! diff -y <(./target/debug/copper "$file") "$file.out"
	else
		! diff -q <(./target/debug/copper "$file") "$file.out"
	fi
}



if [ $# -eq 0 ]; then
	num_tests=0
	num_passed=0

	for file in tests/*.cu; do
		num_tests="$(echo "$num_tests" | awk '{print $1 + 1}')"
		if run-test "$file"; then
			echo "Failed $file"
		else
			num_passed="$(echo "$num_passed" | awk '{print $1 + 1}')"
			echo "Passed $file"
		fi
	done
	echo "Passed $num_passed/$num_tests"
else 
	for t in "$@"; do 
		file="$(ls tests/*.cu | grep "$t" | head -n 1)"
		run-test "$file" 1
	done
fi


