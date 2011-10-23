#!/sw/bin/bash

######################################################################
## Fancy output
######################################################################

# adapted from http://tldp.org/LDP/abs/html/colorizing.html#AEN20111
# and http://tldp.org/LDP/abs/html/sample-bashrc.html

GRAY='\033[1;30m';
black='\033[30;47m'
red='\033[31;47m'
RED='\033[1;31m'
green='\033[32;47m'
GREEN='\033[1;32m';
yellow='\033[33;47m'
blue='\033[34;47m'
BLUE='\033[1;34m'
magenta='\033[35;47m'
cyan='\033[36;47m'
CYAN='\033[1;36m'
white='\033[37;47m'
NC='\033[0m'

function error() {
    local message=$1;
    echo -e "${RED}Error${NC}: $message";
}

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
	error "The required program '$program' could not be found in your path.";
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
	error "The required script '$script' is missing";
	exit 1;
    fi
    if [ ! -r $script ]; then
	error "The required script '$script' is not readable";
	exit 1;
    fi
    if [ ! -x $script ]; then
	error "The required script '$script' is not executable";
	exit 1;
    fi
done

function ensure_sensible_tptp_theory() {
    tptp4X -x -N $1 > /dev/null 2>&1;
    if [ $? -ne "0" ]; then
	error "The TPTP theory at '$1' fails to be a valid TPTP file.";
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
	error "We need an argument to determine whether a file exists and is readable.";
	exit 1;
    fi

    if [ ! -e $1 ]; then
	error "The supplied theory '$1' doesn't exist.";
	exit 1;
    fi

    if [ ! -r $1 ]; then
	error "The supplied theory '$1' is not readable.";
	exit 1;
    fi

}

function run_prover_with_timeout() {

    if [ -z $1 ]; then
	error "A proof script is needed, but none was supplied.";
	return 1;
    fi

    if [ -z $2 ]; then
	error "A theory file is needed, but none was supplied.";
	return 1;
    fi

    if [ -z $3 ]; then
	error "A target proof file must be supplied.";
	return 1;
    fi

    local prover_script=$1;
    local theory=$2;
    local proof=$3;

    $prover_script $theory $prover_timeout > $proof 2> /dev/null;

    return $?;
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

    local conjecture_formula=`tptp4X -N -umachine -c $theory | grep ',conjecture,'`;
    local theory_basename=`basename $theory`;
    local theory=$theory;

    # Let's go!
    echo "--------------------------------------------------------------------------------";

    local prover_name_length=${#prover_name};
    local offset=`expr 80 - $prover_name_length`;
    local indent=`expr $offset / 2`;
    # um
    for ((i=1; i <= $indent; i++)); do
	echo -n " ";
    done
    echo -e "${CYAN}$prover_name${NC}";
    echo "--------------------------------------------------------------------------------";

    # echo -n "* Trying to find a/an $prover_name-proof for $theory_basename...";

    local proof_attempt=1;

    local proof;
    local used_principles;
    local unused_principles;
    local trimmed_theory;
    local num_used_principles;
    local num_unused_principles;

    unused_principles="$prover_directory/$theory_basename.$proof_attempt.proof.unused-principles";

    while [ $proof_attempt = "1" -o -s $unused_principles ]; do

	echo -n "* Proof attempt $proof_attempt..."

	proof="$prover_directory/$theory_basename.$proof_attempt.proof";
	used_principles="$prover_directory/$theory_basename.$proof_attempt.proof.used-principles";
	unused_principles="$prover_directory/$theory_basename.$proof_attempt.proof.unused-principles";
	trimmed_theory="$prover_directory/$theory_basename.$proof_attempt.trimmed";

	run_prover_with_timeout $prover_script $theory $proof;

        # if this didn't work, then don't go any further
	if [ $? -ne "0" ]; then
	    echo -e "${RED}fail${NC}";
	    return 1;
	fi

	$used_principles_script $proof $theory > $used_principles;
	$unused_principles_script $proof $theory > $unused_principles;

	num_used_principles=`cat $used_principles | wc -l | sed -e 's/^ *//'`;
	num_unused_principles=`cat $unused_principles | wc -l | sed -e 's/^ *//'`;

	echo -e "${GREEN}proof found${NC} [${BLUE}$num_used_principles${NC}/${GRAY}$num_unused_principles${NC} principles ${BLUE}used${NC}/${GRAY}unused${NC}]";

	echo "$conjecture_formula" > $trimmed_theory;
	for principle in `cat $used_principles`; do
	    tptp4X -N -umachine -c -x $theory | grep "fof($principle," >> $trimmed_theory;
	done

        ## Sanity check: the theory that we just emitted is a sensible TPTP theory
	ensure_sensible_tptp_theory $trimmed_theory;

	theory=$trimmed_theory;
	proof_attempt=`expr $proof_attempt + 1`;

    done

    return;
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

if [ -f $work_directory ]; then
    error "We would have placed our results into the directory";
    echo
    echo "  $work_directory"
    echo
    echo "but there is already a file in the current working directory"
    echo "by that name. Please move the file out of the way.";
    exit 1;
fi

if [ -d $work_directory ]; then
    error "We would have placed our results into the directory"
    echo
    echo "  $work_directory"
    echo
    echo "but there is already a directory in the current working directory"
    echo "by that name.  Please move the directory out of the way.";
    exit 1;
fi

theory_basename=`basename $theory`;

mkdir -p $work_directory;
cp $theory $work_directory;

echo "================================================================================";
theory_name_length=${#theory_basename};
offset=`expr 80 - $theory_name_length`;
indent=`expr $offset / 2`;
# um
for ((i=1; i <= $indent; i++)); do
    echo -n " ";
done
echo "$theory_basename";
echo "================================================================================";

tptp4X -N -uhuman -c -x $theory

echo "================================================================================";

keep_proving $run_eprover_script $eprover_used_principles_script $eprover_unused_principles_script "eprover";
keep_proving $run_vampire_script $vampire_used_principles_script $vampire_unused_principles_script "vampire";

# Disable prover9.  The construction of prover9 problems is not
# correct.

#keep_proving $run_prover9_script $prover9_used_principles_script $prover9_unused_principles_script "prover9";

echo "================================================================================";
echo "Done.  Our work has been saved in the directory $work_directory.";
