#!/bin/bash -

tptp_to_ladr < $1 | prover9 2> /dev/null | prooftrans renumber
