#!/bin/bash -

proof=$1
theory=$2

tptp4X -umachine $theory | grep ',conjecture,' | cut -f 1 -d ',' | sed -e 's/fof(//';
grep --only-matching '\[input [^[]*\]' $1 | sed -e 's/\[input \(.*\)\]/\1/' | sort -u