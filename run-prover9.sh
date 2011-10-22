#!/bin/bash -

theory=$1;
timeout=$2;

ulimit -t $2;

tptp_to_ladr < $1 | prover9 -x 2> /dev/null | prooftrans renumber
