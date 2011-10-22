#!/bin/bash -

# Rewrite TPTP theorem, definition, assumption, and plain formulas into axioms
function massage_for_vampire() {

    if [ -z $1 ]; then
	echo "Error: a TPTP theory should be supplied";
	exit 1;
    fi

    tptp_theory=$1;

    tptp4X -umachine $tptp_theory \
	| sed -e 's/,definition,/,axiom,/' \
	      -e 's/,theorem,/,axiom,/' \
	      -e 's/,plain,/,axiom,/' \
	      -e 's/,assumption,/,axiom,/' \
	| tptp4X -umachine --;
}

massage_for_vampire $1 | vampire -output_axiom_names on
