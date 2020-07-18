#!/bin/bash
ant jar
test_output=`mktemp`
ntests=0
nsuccess=0
for test in test/*.core
do
    expected_output=$test.output
    if test -r $expected_output -a -f $expected_output
    then
        ntests=`expr $ntests + 1`
        echo "Running test $test ..."
        input=$test.input
        args=$test.args
        if test -r $input -a -f $input ; then
            java -jar dist/skred.jar $test < $input > $test_output
        elif test -r $args -a -f $args ; then
            java -jar dist/skred.jar $test -- `cat $args` > $test_output
        else
            java -jar dist/skred.jar $test > $test_output
        fi
	if test $? -eq 0
        then
            if cmp $expected_output $test_output
            then
                echo "  ok"
                nsuccess=`expr $nsuccess + 1`
            else
                echo "  result different than expected"
            fi
        else
            echo "  failed with exit code $?"
        fi
        rm -f $test_output 
    fi
done
echo "Done: $nsuccess/$ntests successful"

