#!/bin/bash -

if [ -z $1 ]; then
    echo "Usage: `basename $0` AXIOMS [TIMEOUT] [PROMOTE-CONJECTURES-TO-AXIOMS]";
    exit 1;
fi

axiom_file=$1;

if [ ! -e $axiom_file ]; then
    echo "Error: the specified axiom file '$axiom_file' does not exist.";
    exit 1;
fi

if [ ! -f $axiom_file ]; then
    echo "Error: the specified axiom file '$axiom_file' is not a regular file.";
    exit 1;
fi

if [ ! -r $axiom_file ]; then
    echo "Error: the specified axiom file '$axiom_file' is unreadable.";
    exit 1;
fi

timeout=${2-"30"};

promote_conjectures=${2-"no"};

if [ -z "$3" ]; then
    cat $axiom_file | tptp_to_ladr | mace4 -p 1 -S 1 -N 2 -m 1 -s "$timeout";
    exit $?;
else
    cat $axiom_file | sed -e 's/,conjecture,/,axiom,/g' | tptp_to_ladr | mace4 -p 1 -S 1 -m "$timeout";
    exit $?;
fi
