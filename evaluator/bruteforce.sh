#!/bin/sh

for x in {0..100}
do
    echo "Trying row $x ..."
    for y in {0..100}
    do
        cat icfpc-prelude.core.template > icfpc-prelude.core
        echo "  cons $x $y ]" >> icfpc-prelude.core
        v0=$( java -jar build/libs/skred-0.1.0.jar --icfpc2020 galaxy.txt | head -1 | cut -c 1-3 )
        if [ "$v0" = "( 0" ]; then
            :
        else
            echo "WOO! $x $y"
        fi
    done
done
        
