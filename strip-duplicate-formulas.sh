#!/bin/bash -

# TPTP theories that have multiple formulas with identical names are
# invalid, even if the contents of the two formulas are identical.
# This script removes such duplicate formulas from an input TPTP
# theory.

theory=$1;

if [ -z $theory ]; then
    echo "Usage: `basename $0` TPTP-THEORY";
    exit 1;
fi

if [ ! -r $theory ]; then
    echo "Error: the supplied TPTP theory file '$theory' is not readable."
    exit 1;
fi

tptp4X -N -c -x -umachine $theory | sort -u | uniq -u
