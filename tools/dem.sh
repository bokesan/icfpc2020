#!/bin/bash

function neg {
	printf "-"
	pos
}

function pos {
	((index=index+2))
	length=0
	while [ "${input:$index:1}" = "1" ]; do
		((index=index+1))
		((length=length+4))
	done
	((index=index+1))
	if [ "$length" = 0 ]; then
		printf "0"
	else
		number="${input:$index:$length}"
		printf "$((2#$number))"
		((index=index+$length))
	fi
}

function list {
	((index=index+2))
	printf "("
	check
	while [ "${input:$index:2}" = "11" ]; do
		((index=index+2))
		printf ","
		check
	done
	printf ")"
}

function check {
	case "${input:$index:2}" in
	01)
		pos
		;;
	10)
		neg
		;;
	11)
		list
		;;
	00)
		printf "nil "
		;;
	*)
		echo "unknown prefix ${input:$index:2}"
		;;
	esac
}

if [ -z "$1" ]; then
	echo "pass modulated string as argument, for example: 110110000111011111100001001010110101000100"
	exit 1	
fi

input=$1
index=0
check
printf "\n"


