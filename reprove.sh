#!/bin/bash -

######################################################################
## Sanity checking
######################################################################

# Check that all the programs that we use here exist and are executable

vampire_programs="vampire";
eprover_programs="eprove epclextract";
prover9_programs="tptp_to_ladr prover9 prooftrans"
tptp_programs="tptp4X";

needed_programs="$vampire_programs $eprover_programs $prover9_programs $tptp_programs";

for program in $needed_programs; do
    which $program > /dev/null;
    if [ $? -ne "0" ]; then
	echo "Error: the required program '$program' could not be found in your path.";
	exit 1;
    fi
done

script_home=`dirname $0`; # blah

run_eprover_script="$script_home/run-eprover.sh";
eprover_used_principles_script="$script_home/eprover-used-principles.sh";
eprover_unused_principles_script="$script_home/eprover-unused-principles.sh";
eprover_scripts="$run_eprover_script $eprover_used_principles_script $eprover_unused_principles_script"

run_vampire_script="$script_home/run-vampire.sh";
vampire_used_principles_script="$script_home/vampire-used-principles.sh";
vampire_unused_principles_script="$script_home/vampire-unused-principles.sh";
vampire_scripts="$run_vampire_script $vampire_used_principles_script $vampire_unused_principles_script"

run_prover9_script="$script_home/run-prover9.sh";
prover9_used_principles_script="$script_home/prover9-used-principles.sh";
prover9_unused_principles_script="$script_home/prover9-unused-principles.sh";
prover9_scripts="$run_prover9_script $prover9_used_principles_script $prover9_unused_principles_script"

tptp_scripts="$script_home/tptp-labels.sh";

scripts="$eprover_scripts $vampire_scripts $prover9_scripts $tptp_scripts";

for script in $scripts; do
    if [ ! -e $script ]; then
	echo "Error: the required script '$script' is missing";
	exit 1;
    fi
    if [ ! -r $script ]; then
	echo "Error: the required script '$script' is not readable";
	exit 1;
    fi
    if [ ! -x $script ]; then
	echo "Error: the required script '$script' is not executable";
	exit 1;
    fi
done

function ensure_sensible_tptp_theory() {
    tptp4X $1 > /dev/null 2>&1;
    if [ $? -ne "0" ]; then
	echo "The TPTP theory at '$1' fails to be a valid TPTP file.";
	exit 1;
    fi
}

######################################################################
## Notable global parameters
######################################################################

# The timeout used to stop a prover.
prover_timeout="30"; # seconds

function ensure_file_exists_and_is_readable() {

    if [ -z $1 ]; then
	echo "Error: we need an argument to determine whether a file exists and is readable";
	exit 1;
    fi

    if [ ! -e $1 ]; then
	echo "Error: the supplied theory '$1' doesn't exist";
	exit 1;
    fi

    if [ ! -r $1 ]; then
	echo "Error: the supplied theory '$1' is not readable";
	exit 1;
    fi

}

function run_prover_with_timeout() {

    if [ -z $1 ]; then
	echo "Error: a proof script is needed, but none was supplied";
	return 1;
    fi

    if [ -z $2 ]; then
	echo "Error: a theory file is needed, but none was supplied";
	return 1;
    fi

    if [ -z $3 ]; then
	echo "Error: a target proof file must be supplied";
	return 1;
    fi

    local prover_script=$1;
    local theory=$2;
    local proof=$3;

    $prover_script $theory $prover_timeout > $proof;

    # Mac OS X timeout is weird
    if [ $? -eq "124" ]; then
	echo; # because the previous call to echo used -n
	echo "Unable to find an initial proof in less than $prover_timeout; unable to proceed.";
	exit 1;
    fi
    return;
}

# $1: the script to be executed (under a timeout)
#
# $2: the script that will tell us what principles were used in the
#     proof
#
# $3: the script that will tell us what principles were *not* used in
# the proof
#
# $4: the name of the subdirectory of $work_directory where we will
#     save our output
function keep_proving() {

    local prover_script=$1;
    local used_principles_script=$2;
    local unused_principles_script=$3;
    local prover_name=$4;

    local prover_directory=$work_directory/$prover_name;
    mkdir -p $prover_directory;

    local theory_basename=`basename $theory`;
    local theory=$theory;

    local proof_attempt=1;

    local proof="$prover_directory/$theory_basename.1.proof";
    local used_principles="$prover_directory/$theory_basename.1.proof.used-principles";
    local unused_principles="$prover_directory/$theory_basename.1.proof.unused-principles";
    local trimmed_theory="$prover_directory/$theory_basename.1.trimmed";

    # do the initial proof
    run_prover_with_timeout $prover_script $theory $proof;

    # if this didn't work, then don't go any further
    if [ $? -ne "0" ]; then
	echo "Error: the initial run of the proof script";
	echo;
	echo "  $prover_script";
	echo;
	echo "did not exit cleanly when applied to";
	echo;
	echo "  $theory";
	echo;
	echo "so we cannot continue.";
	return 1;
    fi

    # compute used and unused principles, and trim the theory
    $used_principles_script $proof $theory > $used_principles;
    $unused_principles_script $proof $theory > $unused_principles;

    for principle in `cat $used_principles`; do
	tptp4X -umachine $theory | grep "fof($principle," >> $trimmed_theory;
    done

    ## Sanity check: the theory that we just emitted is a sensible TPTP theory
    ensure_sensible_tptp_theory $trimmed_theory;

    while [ -s $unused_principles -a $proof_attempt -lt "5" ]; do

	theory=$trimmed_theory;
	proof_attempt=`expr $proof_attempt + 1`;
	proof="$prover_directory/$theory_basename.$proof_attempt.proof";

	run_prover_with_timeout $prover_script $theory $proof;

        # if this didn't work, then don't go any further
	if [ $? -ne "0" ]; then
	    echo "Error: run number $proof_attempt of the proof script";
	    echo;
	    echo "  $prover_script";
	    echo;
	    echo "did not exit cleanly when applied to";
	    echo;
	    echo "  $theory";
	    echo;
	    echo "so we cannot continue.";
	    return 1;
	fi

	used_principles="$prover_directory/$theory_basename.$proof_attempt.proof.used-principles";
	unused_principles="$prover_directory/$theory_basename.$proof_attempt.proof.unused-principles";

	$used_principles_script $proof $trimmed_theory > $used_principles;
	$unused_principles_script $proof $trimmed_theory > $unused_principles;

	trimmed_theory="$prover_directory/$theory_basename.$proof_attempt.trimmed";

	for principle in `cat $used_principles`; do
	    tptp4X -umachine $theory | grep "fof($principle," >> $trimmed_theory;
	done

        ## Sanity check: the theory that we just emitted is a sensible TPTP theory
	ensure_sensible_tptp_theory $trimmed_theory;

    done

    echo "We needed $proof_attempt iteration(s) before finding the $prover-minimal set of principles";

    return;
}

# $1: the script to be executed (under a timeout)
#
# $2: the script that will tell us what principles were used in the
#     proof
#
# $3: the script that will tell us what principles were *not* used in
# the proof
#
# $4: the name of the subdirectory of $work_directory where we will
#     save our output
function reprove() {

    local prover_script=$1;
    local used_principles_script=$2;
    local unused_principles_script=$3;
    local prover_name=$4;

    local prover_directory=$work_directory/$prover_name;

    mkdir -p $prover_directory;

    local theory=`basename $theory`;

    local first_proof=$prover_directory/$theory.original;
    local first_proof_principles=$first_proof.used-principles;
    local first_proof_unused_principles=$first_proof.unused-principles
    local trimmed_theory=$prover_directory/$theory.trimmed.tptp;
    local second_proof=$prover_directory/$theory.trimmed;
    local second_proof_principles=$second_proof.used-principles;
    local second_proof_unused_principles=$second_proof.unused-principles;

    echo -n "Trying to find first proof for '$theory' using $prover_name...";
    run_prover_with_timeout $prover_script $theory $first_proof;

    $used_principles_script $first_proof $theory > $first_proof_principles;
    $unused_principles_script $first_proof $theory > $first_proof_unused_principles;

    echo "done."

    for principle in `cat $first_proof_principles`; do
	tptp4X -umachine $theory | grep "fof($principle," >> $trimmed_theory;
    done

    ## Sanity check: the theory that we just emitted is a sensible TPTP theory
    ensure_sensible_tptp_theory $trimmed_theory;

    echo -n "Trying to find a second proof from the trimmed theory...";

    run_prover_with_timeout $prover_script $trimmed_theory $second_proof;

    $used_principles_script $second_proof $theory > $second_proof_principles;
    $unused_principles_script $second_proof $trimmed_theory > $second_proof_unused_principles;

    echo "done."
}

######################################################################
## Check commandline arguments
######################################################################

if [ ! -n "$1" ]
then
  echo "Usage: `basename $0` THEORY-FILE WORK-DIRECTORY"
  exit 1;
fi

if [ ! -n "$2" ]
then
  echo "Usage: `basename $0` THEORY-FILE WORK-DIRECTORY"
  exit 1;
fi

theory=$1;
work_directory=$2;

ensure_file_exists_and_is_readable $theory;
ensure_sensible_tptp_theory $theory;

if [ -e $work_directory ]; then
    echo "Error: We would have placed our results into the directory";
    echo
    echo "  $work_directory"
    echo
    echo "but there is already a file in the current working directory by that name."
    echo "Please move the file out of the way.";
    exit 1;
fi

if [ -d $work_directory ]; then
    echo "Error: We would have placed our results into the directory"
    echo
    echo "  $work_directory"
    echo
    echo "but there is already a directory in the current working directory by that name."
    echo "Please move the directory out of the way.";
    exit 1;
fi

mkdir -p $work_directory;
cp $theory $work_directory;

keep_proving $run_eprover_script $eprover_used_principles_script $eprover_unused_principles_script "eprover";
keep_proving $run_vampire_script $vampire_used_principles_script $vampire_unused_principles_script "vampire";
keep_proving $run_prover9_script $prover9_used_principles_script $prover9_unused_principles_script "prover9";

exit 0;
