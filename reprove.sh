#!/bin/bash -

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
WHITE='\033[1;37m'
purple='\033[0;35m'
PURPLE='\033[1;35m'
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
eprover_programs="eprover epclextract";
prover9_programs="tptp_to_ladr prover9 prooftrans mace4"
tptp_programs="tptp4X tptp2X";

needed_programs="$vampire_programs $eprover_programs $prover9_programs $tptp_programs";

for program in $needed_programs; do
    which $program > /dev/null;
    if [ $? -ne "0" ]; then
        error "The required program '$program' could not be found in your path.";
        exit 1;
    fi
done

script_home=`dirname $0`; # blah

provers="vampire eprover prover9";

function script_for_prover() {
    echo "$script_home/run-$1.sh";
}

function used_principles_script_for_prover() {
    echo "$script_home/$1-used-principles.sh";
}

function unused_principles_script_for_prover() {
    echo "$script_home/$1-unused-principles.sh";
}

function sentry_script_for_prover() {
    echo "$script_home/$1-sentry.pl";
}

scripts="$eprover_scripts $vampire_scripts $prover9_scripts $tptp_scripts";

for prover in "$provers"; do
    run_prover_script=`script_for_prover $prover`;
    used_principles_script=`used_principles_script_for_prover $prover`;
    unused_principles_script=`unused_principles_script_for_prover $prover`;
    sentry_script=`sentry_script_for_prover $prover`;
    for script in $run_prover_script $used_principles_script $unused_principles_script $sentry_script; do
        if [ ! -e "$script" ]; then
            error "The required script '$script' is missing from $script_home";
            exit 1;
        fi
        if [ ! -r "$script" ]; then
            error "The required script '$script' is not readable";
            exit 1;
        fi
        if [ ! -x "$script" ]; then
            error "The required script '$script' is not executable";
            exit 1;
        fi
    done
done

function ensure_sensible_tptp_theory() {
    tptp4X -c -x -N -V $1 > /dev/null 2>&1;
    if [ $? -ne "0" ]; then
        local error_message=`tptp4X -c -x -N -V $1`;
        error "The TPTP theory at '$1' fails to be a valid TPTP file.";
        echo "The error message from the tptp4X tool is:";
        echo "$error_message";
        exit 1;
    fi

    # tptp4x, when called with -x, will print "ERROR: cannot open ..."
    # when it can't open the includes.  But then it exits cleanly!

    local unable_to_open=`tptp4X -x -N -V $1 | grep 'ERROR: Cannot open'`;

    if [ ! -z "$unable_to_open" ]; then
        error "The TPTP theory at '$1' has include directives that cannot be processed.";
        echo "Here is the error message from TPTP4X:";
        echo "$unable_to_open";
        exit 1;
    fi

    # Make sure that no formula is called 'conjecture' or 'axiom'
    tptp4X -x -N -V -umachine $1 | grep --silent '^fof(conjecture,';

    if [ $? -eq "0" ]; then
        error "There is a formula in the given TPTP theory called 'conjecture'.  Please rename it.";
        exit 1;
    fi

    tptp4X -x -N -V -umachine $1 | grep --silent '^fof(axiom,';

    if [ $? -eq "0" ]; then
        error "There is a formula in the given TPTP theory called 'axiom'.  Please rename it.";
        exit 1;
    fi

    local conjecture=`tptp4X -x -N -V -umachine $1 | grep --count ',conjecture,'`;
    if [ "$conjecture" -eq "0" ]; then
        error "The TPTP theory at '$1' contains no conjecture formula.";
        exit 1;
    elif [ "$conjecture" -gt "1" ]; then
        error "The TPTP theory at '$1' has more than one conjecture formula.";
        exit 1;
    fi
    return 0;
}

######################################################################
## Notable global parameters
######################################################################

# The timeout used to stop a prover.
prover_timeout="30"; # seconds
model_finder_timeout="5"; # seconds

function ensure_file_exists_and_is_readable() {

    if [ -z "$1" ]; then
        error "We need an argument to determine whether a file exists and is readable.";
        exit 1;
    fi

    if [ -d "$1" ]; then
        error "The supplied theory '$1' is already the name of a directory.";
        exit 1;
    fi

    if [ ! -e "$1" ]; then
        error "The supplied theory '$1' doesn't exist.";
        exit 1;
    fi

    if [ ! -r "$1" ]; then
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

    if [ -z $4 ]; then
        error "An error file must be supplied.";
    fi

    local prover_script=$1;
    local theory=$2;
    local proof=$3;
    local errors=$4;

    $prover_script $theory $prover_timeout > $proof 2> $errors;

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

    local conjecture_formula=`tptp4X -V -N -umachine -c $theory | grep ',conjecture,'`;
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
        error_log="$prover_directory/$theory_basename.$proof_attempt.proof.errors";
        used_principles="$prover_directory/$theory_basename.$proof_attempt.proof.used-principles";
        unused_principles="$prover_directory/$theory_basename.$proof_attempt.proof.unused-principles";
        trimmed_theory="$prover_directory/$theory_basename.$proof_attempt.trimmed";

        run_prover_with_timeout $prover_script $theory $proof $error_log;

        prover_exit_code="$?";

        # if this didn't work, then don't go any further
        if [ $prover_exit_code -eq "0" ]; then
            # was any proof emitted?
            if [ ! -s $proof ]; then
                echo -e "${RED}fail${NC}";
                return 1;
            fi
        else
            if [ $prover_exit_code -eq "2" ]; then
                echo -e "${RED}countersatisfiable${NC}";
                return 1;
            elif [ $prover_exit_code -eq "3" ]; then
                echo -e "${RED}timeout${NC}";
                return 1;
            else
                echo -e "${RED}fail${NC}";
                return 1;
            fi
        fi

        $used_principles_script $proof $theory > $used_principles;
        $unused_principles_script $proof $theory > $unused_principles;

        num_used_principles=`cat $used_principles | wc -l | sed -e 's/^ *//'`;
        num_unused_principles=`cat $unused_principles | wc -l | sed -e 's/^ *//'`;

        if [ -s $used_principles ]; then
            echo -e "${GREEN}proof found${NC} [${BLUE}$num_used_principles${NC}/${GRAY}$num_unused_principles${NC} principles ${BLUE}used${NC}/${GRAY}unused${NC}]";
        else
            echo -e "${GREEN}proof found${NC} [${RED}$num_used_principles${NC}/${GRAY}$num_unused_principles${NC} principles ${RED}used${NC}/${GRAY}unused${NC}]";
        fi

        echo "$conjecture_formula" > $trimmed_theory;
        for principle in `cat $used_principles`; do
            tptp4X -V -N -umachine -c -x $theory | grep "fof($principle," >> $trimmed_theory;
        done

        ## Sanity check: the theory that we just emitted is a sensible TPTP theory
        ensure_sensible_tptp_theory $trimmed_theory;

        theory=$trimmed_theory;
        proof_attempt=`expr $proof_attempt + 1`;

    done

    return 0;
}

######################################################################
## Check commandline arguments
######################################################################

if [ ! -n "$1" ]
then
  echo "Usage: `basename $0` THEORY-FILE [WORK-DIRECTORY]"
  exit 1;
fi

theory=$1;

theory_basename=`basename $theory`;
theory_basename_sans_extension=`echo "$theory_basename" | sed -e 's/\(.*\)\.[^.]*$/\1/'`;
axiom_file="$theory_basename_sans_extension.ax";

theory_dirname=`dirname $theory`;

if [ -z "$2" ]; then
    try=1;
    candidate_work_directory="${theory_dirname}/${theory_basename_sans_extension}-${try}";
    while [ -e "$candidate_work_directory" -o -d "$candidate_work_directory" ]; do
        try=`expr $try + 1`;
        candidate_work_directory="${theory_dirname}/${theory_basename_sans_extension}-${try}";
    done
    work_directory="$candidate_work_directory";
else
    work_directory=$2;
fi

ensure_file_exists_and_is_readable $theory;
ensure_sensible_tptp_theory $theory;

if [ -z "$work_directory" ]; then
    error "We have failed to compute a sensible work directory.";
    exit 1;
fi

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

mkdir -p $work_directory;

echo "================================================================================";
theory_name_length=${#theory_basename};
offset=`expr 80 - $theory_name_length`;
indent=`expr $offset / 2`;
# um
for ((i=1; i <= $indent; i++)); do
    echo -n " ";
done
echo -e "${PURPLE}$theory_basename${NC}";
echo "================================================================================";

# Save the axioms (non-conjecture formulas) of the theory in a
# separate file
tptp4X -N -V -c -x -umachine $theory \
    | grep --invert-match ',conjecture,' \
    | tptp4X -uhuman -- > "$work_directory/$axiom_file";

echo "                              Consistency Check";

echo "================================================================================";

# Consistency checking, 1: the axioms are consistent

echo -n "Axioms alone..........";

axiom_model_file="$work_directory/$axiom_file.model";
axiom_model_errors="$work_directory/$axiom_file.model.errors";
axiom_model_file_basename=`basename $axiom_model_file`;

$mace4_script "$work_directory/$axiom_file" $model_finder_timeout > $axiom_model_file 2> $axiom_model_errors;

if [ $? -eq "0" ]; then
    echo -e "${GREEN}satisfiable${NC} (saved in $axiom_model_file_basename)";
else
    echo -e "${RED}unknown${NC}"
fi

# Consistency checking, 2: the axioms + the conjecture are consistent

echo -n "Axioms + conjecture...";

whole_problem_model_file="$work_directory/$theory_basename.model";
whole_problem_model_errors="$work_directory/$theory_basename.model.errors";
whole_problem_model_file_basename=`basename $whole_problem_model_file`;

$mace4_script "$work_directory/$axiom_file" $model_finder_timeout 1 > $whole_problem_model_file 2> $whole_problem_model_errors;
# promote conjecture(s) to axioms                                 ^

if [ $? -eq "0" ]; then
    echo -e "${GREEN}satisfiable${NC} (saved in $whole_problem_model_file_basename)";
else
    echo -e "${RED}unknown${NC}";
fi

echo "================================================================================";

for prover in $provers; do
    run_prover_script=`script_for_prover $prover`;
    used_principles_script=`used_principles_script_for_prover $prover`;
    unused_principles_script=`unused_principles_script_for_prover $prover`;
    keep_proving $run_prover_script $used_principles_script $unused_principles_script $prover;
done

echo "================================================================================";
echo "Done.  Our work has been saved in the directory $work_directory.";
