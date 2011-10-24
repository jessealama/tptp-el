#!/bin/bash -

theory=$1;
timeout=${2-"30"};

if [ -z $1 ]; then
    echo "Usage: `basename $0` TPTP-THEORY [TIMEOUT]";
    exit 1;
fi

if [ ! -r $theory ]; then
    echo "Error: the supplied TPTP theory '$theory' is not readable";
    exit 1;
fi

ulimit -t $timeout \
    || (echo "Error: '$timeout' is not an acceptable argument to ulimit -t." && exit 1);

# Rewrite TPTP theorem, definition, assumption, and plain formulas
# into axioms.  Vampire complains if multiply-defined formulas occur
# in its input theory, so we uniq'ify them.
function massage_for_vampire() {
    tptp4X -N -umachine $1 \
	| sed -e 's/,definition,/,axiom,/' \
	      -e 's/,theorem,/,axiom,/' \
	      -e 's/,plain,/,axiom,/' \
	      -e 's/,assumption,/,axiom,/' \
	| sort -u \
        | uniq -u;
}

massage_for_vampire $1 | vampire -output_axiom_names on | grep '^[1-9][0-9]*\.'
