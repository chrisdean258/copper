#!/bin/bash


cargo build || exit 1

run-test()
{
	diff -y <(./target/debug/copper "$1" 2>&1) "$1.out"
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
		if run-test "$file" &>/dev/null; then
			num_passed="$((num_passed + 1))"
			color 2 "Passed $file"
		else
			color 1 "Failed $file"
		fi
	done
	echo "Passed $num_passed/$num_tests"
else 
	for t in "$@"; do 
		file="$(find ./test -name '*.cu' | grep "$t" | head -n 1)"
		run-test "$file"
	done
fi


