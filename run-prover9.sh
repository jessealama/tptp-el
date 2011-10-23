#!/bin/bash -

theory=$1;
timeout=$2;

ulimit -t $timeout \
    || (echo "Error: '$timeout' is not an acceptable argument to ulimit -t." && exit 1);

tptp_to_ladr < $theory | prover9 -x 2> /dev/null | prooftrans renumber
