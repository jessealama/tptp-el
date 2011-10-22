#!/bin/bash -

proof=$1;
theory_of_proof=$2;
for formula in `tptp4X -c -x -umachine $theory_of_proof | cut -f 1 -d ',' | sed -e 's/fof(//'`; do
    grep --silent " initial(.*, $formula)" $proof > /dev/null 2>&1;
    if [ $? -ne "0" ]; then echo $formula; fi
done