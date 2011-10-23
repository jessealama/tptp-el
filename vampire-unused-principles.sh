#!/bin/bash -

vampire_proof=$1;
theory=$2;

if [ -z $vampire_proof ]; then
    echo "Usage: `basename $0` VAMPIRE-PROOF-OBJECT TPTP-BACKGROUND-THEORY";
    exit 1;
fi

if [ -z $theory ]; then
    echo "Usage: `basename $0` VAMPIRE-PROOF-OBJECT TPTP-BACKGROUND-THEORY";
    exit 1;
fi

for formula in `tptp4X -c -x -umachine $theory | grep --invert-match ',conjecture,' | cut -f 1 -d ',' | sed -e 's/fof(//'`; do
    grep --silent "\[input $formula\]" $vampire_proof;
    if [ $? -ne "0" ]; then echo $formula; fi
done
