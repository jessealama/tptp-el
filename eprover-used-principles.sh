#!/bin/bash -

grep --only-matching ' initial(.*$' $1 \
    | cut -f 2 -d ',' \
    | cut -f 1 -d ')' \
    | sed -e 's/^ //' \
    | sort -u
