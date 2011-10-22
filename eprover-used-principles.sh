#!/bin/bash -

proof=$1
background_theory=$2  # we don't actually need this; the proof object
		      # emitted by E contain all the information we
		      # need, so there's no need to consult the
		      # background theory from which the proof came

grep --only-matching ' initial(.*$' $proof \
    | cut -f 2 -d ',' \
    | cut -f 1 -d ')' \
    | sed -e 's/^ //' \
    | sort -u
