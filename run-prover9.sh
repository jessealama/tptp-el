#!/bin/bash -

theory=$1;
timeout=$2;

ulimit -t $timeout;

tptp_to_ladr < $theory | prover9 -x 2> /dev/null | prooftrans renumber
