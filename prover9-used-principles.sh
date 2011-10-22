#!/bin/bash -

# Working with a prover9 output line such as
#
# 10 (all A all B (r1_xboole_0(A,B) <-> k3_xboole_0(A,B) = k1_xboole_0)) # label(d7_xboole_0) # label(definition) # label(non_clause).  [assumption].
#
# or
#
# 21 -r1_xboole_0(c1_14__xboole_0,c2_14__xboole_0) # label(e1_14_1__xboole_0) # label(assumption) # answer(e1_14_1__xboole_0).  [assumption].
#
# corresponding to an assumption, extract the labels and answers.

enumerate_labels_awk_script='BEGIN { FS = "#"; } /./ { for (i = 2; i <= NF; i++) { print $i; } }';
#                                    ^^ field separator     ^^^^^ start at field 2 because field 1 is everything before the first label

function prover9_assumptions() {
    local prover9_proof=$1;
    grep '[assumption]' $prover9_proof \
	| sed -e 's/\[assumption\]//' \
              -e 's/\.//g' \
	      -e 's/\[.*\]//';
}

function prover9_labels_and_answers() {
    local prover9_proof=$1;
    prover9_assumptions $prover9_proof \
	| gawk "$enumerate_labels_awk_script" \
	| sed -e 's/label(\(.*\))/\1/' \
	      -e 's/answer(\(.*\))/\1/' \
	      -e 's/^ *//' -e 's/ *$//' \
	| sort -u;
}

prover9_proof=$1;
theory=$2
for principle in `prover9_labels_and_answers $prover9_proof`; do
    grep --silent "^fof($principle," $theory > /dev/null 2>&1;
    if [ $? -eq "0" ]; then echo $principle; fi
done
