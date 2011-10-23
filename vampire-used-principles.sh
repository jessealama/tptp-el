#!/bin/bash -

proof=$1
theory=$2

grep --only-matching '\[input [^[]*\]' $1 \
    | sed -e 's/\[input \(.*\)\]/\1/' \
    | sort -u
