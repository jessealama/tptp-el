#!/bin/bash -

proof=$1
background_theory=$2

function used() {
    grep --only-matching ' initial(.*$' $proof \
	| cut -f 2 -d ',' \
	| cut -f 1 -d ')' \
	| sed -e 's/^ //' \
	| sort -u;
}

for formula in `used`; do
    grep --silent "fof($formula,conjecture," $background_theory > /dev/null 2>&1;
    if [ $? -ne "0" ]; then echo $formula; fi
done
