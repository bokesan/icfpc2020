#!/bin/bash

#!/bin/bash

function neg {
	printf "10"
	((index=index+1))
	number
}

function pos {
	printf "01"
	number
}

function number {
	length=0
	while [[ "${input:$index+$length:1}" == [0-9] ]]; do
		((length=length+1))
	done
	number=${input:$index:$length}
	((index=index+length))
	local n bit
    	for (( n=$number ; n>0 ; n >>= 1 )); do 
		bit="$(( n&1 ))$bit"
	done
	while [ "$((${#bit}%4))" != "0" ]; do
		bit="0$bit"
	done
	size=${#bit}
	while [ $size -gt 0 ]; do
		((size=size-4))
		printf "1"
	done
	printf "0"
	printf "$bit"
}

function list {
	((index=index+1))
	printf "11"
	check
	while [ "${input:$index:1}" = "," ]; do
		((index=index+1))
		printf "11"
		check
	done
	printf "00"
}

function cons {
	((index=index+1))
	printf "11"
	check
	while [ "${input:$index:1}" = "," ]; do
		((index=index+1))
		check
	done
	((index=index+1))
}

function check {
	case "${input:$index:1}" in
	[0-9])
		pos
		;;
	-)
		neg
		;;
	"(")
		list
		;;
	"[")
		cons
		;;
	*)
		echo "unknown element ${input:$index:1}"
		;;
	esac
}

if [ -z "$1" ]; then
	echo 'pass unmodulated string as argument, for example: "(1,76625)"'
	exit 1	
fi

input=$1
index=0
check
printf "\n"


