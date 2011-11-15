#!/usr/bin/perl -w

use strict;

use Term::ANSIColor;

use POSIX qw(floor);

sub copy_string {
  my $string = shift;
  my $num_copies = shift;
  my $final_string = '';
  foreach my $i (1 .. $num_copies) {
    $final_string .= $string;
  }
  return $final_string;
}

sub max {
  my $a = shift;
  my $b = shift;
  $a > $b ? $a : $b;
}

if (scalar @ARGV == 0) {
  print 'Usage: reprove-semantically TPTP-THEORY', "\n";
  exit 1;
}

if (scalar @ARGV > 1) {
  print 'Usage: reprove-semantically TPTP-THEORY', "\n";
  exit 1;
}

my $tptp_theory = $ARGV[0];

if (! -e $tptp_theory) {
  print 'Error: the supplied file ', $tptp_theory, ' does not exist.', "\n";
  exit 1;
}

if (-d $tptp_theory) {
  print 'Error: the supplied file ', $tptp_theory, ' is actually a directory.', "\n";
  exit 1;
}

if (! -r $tptp_theory) {
  print 'Error: the TPTP theory at ', $tptp_theory, ' is unreadable.', "\n";
  exit 1;
}

sub extract_formula_from_fof {
  my $fof = shift;
  if (defined $fof) {
    # the next pattern is not robust: it works only for some TPTP fof
    # lines, not all.  It assumes that the fof line has exactly three
    # parts: the formula name, the formula status, and the formula
    # proper.  But TPTP syntax permits multiple fields after the
    # formula proper.  Do we need a TPTP parser?  *sigh*
    $fof =~ m/fof\([^,]+,[^,]+,(.+)\).$/;
    my $formula = $1;
    # sanity check
    if (defined $formula) {
      return $formula;
    } else {
      warn "We were unable to extract the formula from '$fof'";
      return '';
    }
  } else {
    die "We were given an empty argument to extract_formula_from_fof";
  }
}

sub extract_name_from_fof {
  my $fof = shift;
  $fof =~ m/fof\(([^,]+),/;
  my $name = $1;
  # sanity check
  if (defined $name) {
    return $name;
  } else {
    warn "We were unable to extract the formula name from '$fof'";
    return '';
  }
}

my @conjecture_fofs
  = `tptp4X -N -V -c -x -umachine $tptp_theory | grep ',conjecture,'`;
chomp @conjecture_fofs;
my @non_conjecture_fofs
  = `tptp4X -N -V -c -x -umachine $tptp_theory | grep --invert-match ',conjecture,'`;
chomp @non_conjecture_fofs;

if (scalar @conjecture_fofs > 1) {
  print 'Error: there is more than one conjecture formula in the given theory.', "\n";
  exit 1;
}

my $conjecture_fof
  = scalar @conjecture_fofs == 0 ? undef : $conjecture_fofs[0];
my $conjecture = undef;
my $negated_conjecture = undef;

# explicitly negate the conjecture, if there is one
if (defined $conjecture_fof) {
  $conjecture = extract_formula_from_fof ($conjecture_fof);
  if ($conjecture eq '') {
    warn "Unable to extract the formula from the conjecture fof!";
  } else {
    $negated_conjecture = '~ ( ' . $conjecture . ')';
  }
}

my @non_conjecture_formulas = map { extract_formula_from_fof($_); } @non_conjecture_fofs;
my @non_conjecture_names = map { extract_name_from_fof($_); } @non_conjecture_fofs;

my %needed_formulas = ();

my $length_of_longest_premise = 0;

foreach my $premise (@non_conjecture_names) {
  my $len = length $premise;
  if ($len > $length_of_longest_premise) {
    $length_of_longest_premise = $len;
  }
}

my $bigger = max (length ('Premise'), $length_of_longest_premise);
my $padding = $bigger == length ('Premise') ? 0 : $length_of_longest_premise - length ('Premise');
print 'Premise', copy_string (' ', $padding), ' | ', 'Needed according to mace4', ' | ', 'Needed according to paradox', "\n";
print copy_string ('=', length ('Premise') + $padding + length (' | ') + length ('Needed according to mace4') + length (' | ') + length ('Needed according to paraox')), "\n";

foreach my $i (1 .. scalar @non_conjecture_formulas) {
  my $non_conjecture_fof = $non_conjecture_fofs[$i-1];
  my $non_conjecture_name = $non_conjecture_names[$i-1];
  open (AD_HOC_THEORY, '>', "splork")
    or die "Can't write to splork";
  foreach my $j (1 .. scalar @non_conjecture_formulas) {
    unless ($i == $j) {
      print AD_HOC_THEORY ('fof(' . "ax$j" . ',axiom,' . $non_conjecture_formulas[$j-1] . ').' . "\n");
    }
    if (defined $negated_conjecture) {
      print AD_HOC_THEORY ('fof(our_conjecture,axiom,' . $negated_conjecture . ').' . "\n");
    }
  }
  close AD_HOC_THEORY
    or die "Can't close the output filehandle for splork";
  my $length_of_this_premise = length $non_conjecture_name;
  my $padding = $length_of_longest_premise - $length_of_this_premise;
  print colored ($non_conjecture_name, 'blue'), copy_string (' ', $padding), ' | ';

  my $mace4_status = system ("tptp_to_ladr < splork | mace4 -p 0 -n 2 -S 1 -s 5 > /dev/null 2>&1");
  my $mace4_exit_code = $mace4_status >> 8;
  if ($mace4_exit_code == 0) {
    print colored ('needed', 'red'), '                   ';
    $needed_formulas{$non_conjecture_fof} = 0;
  } else {
    print colored ('possibly not needed', 'cyan'), '      ';
  }

  print ' | ';

  my $paradox_status = system ("run-paradox.sh splork 5 > /dev/null");
  my $paradox_exit_code = $paradox_status >> 8;
  if ($paradox_exit_code == 4) {
    print colored ('needed', 'red');
    $needed_formulas{$non_conjecture_fof} = 0;
  } else {
    print colored ('possibly not needed', 'cyan');
  }

  print "\n";
}

# Test whether the formulas that we found were needed suffice for a proof

my $maybe_minimal_theory = "$tptp_theory.maybe-semantically-minimal";

print "Compiling the semantically needed formulas into '$maybe_minimal_theory'...";

open (MINIMAL_THEORY, '>', $maybe_minimal_theory)
  or die "Can't open '$maybe_minimal_theory'";

foreach my $fof (keys %needed_formulas) {
  print MINIMAL_THEORY ("$fof\n");
}

if (defined $conjecture) {
  print MINIMAL_THEORY ('fof(our_conjecture,conjecture,' . $conjecture . ').' . "\n");
}

close MINIMAL_THEORY
  or die "Can't close '$maybe_minimal_theory'";

print "done.\n";

print "Now we will test whether the 'semantically minimal' subtheory of the original theory suffices to prove the conjecture.\n";

# The semantically minimal theory might be countersatisfiable

print 'First, we will use a model finder to check whether the theory we just constructed is countersatisfiable.', "\n";

print '* mace4...';

my $mace4_status = system ("run-mace4.sh $maybe_minimal_theory 5 > /dev/null 2>&1");
my $mace4_exit_code = $mace4_status >> 8;

if ($mace4_exit_code == 0) {
  print colored ('countersatisfiable', 'red'), '!  (Some further principle is needed.)', "\n";
} else {
  print colored ('unknown', 'cyan'), '.', "\n";
  print '* paradox...';
  my $paradox_status = system ("run-paradox.sh $maybe_minimal_theory 5 > /dev/null 2>&1");
  my $paradox_exit_code = $paradox_status >> 8;
  if ($paradox_exit_code == 2) {
    print colored ('countersatisfiable', 'red'), '!  (Some further principle is needed.)', "\n";
  } else {
    print colored ('unknown', 'cyan'), '.', "\n";
    print 'Unable to detect countersatisfiability using a model finder.  Switching now to theorem provers.', "\n";
    print '* eprover...';
    my $eprover_status = system ("run-eprover.sh $maybe_minimal_theory > /dev/null 2>&1");
    my $eprover_exit_code = $eprover_status >> 8;
    if ($eprover_exit_code == 0) {
      print colored ('confirmed', 'green'), '!', "\n";
    } elsif ($eprover_exit_code == 2) {
      print colored ('countersatisfiable', 'red'), '!  (Some further principle is needed.)', "\n";
    }

    # Try vampire
    print '* vampire...';
    my $vampire_status = system ("run-vampire.sh $maybe_minimal_theory > /dev/null 2>&1");
    my $vampire_exit_code = $vampire_status >> 8;
    if ($vampire_exit_code == 0) {
      print colored ('confirmed', 'green'), '!', "\n";
    } elsif ($vampire_exit_code == 2) {
      print colored ('countersatisfiable', 'red'), '!  (Some further principle is needed.)', "\n";
    }

    print '* prover9...';
    my $prover9_status = system ("run-prover9.sh $maybe_minimal_theory > /dev/null 2>&1");
    my $prover9_exit_code = $prover9_status >> 8;
    if ($prover9_exit_code == 0) {
      print colored ('confirmed', 'green'), '!', "\n";
    } elsif ($prover9_exit_code == 2) {
      print colored ('countersatisfiable', 'red'), '! (Some further principle is needed.)', "\n";
    } else {
      print colored ('unknown', 'cyan'), '.', "\n";
    }
  }
}

exit 0;
