#!/usr/bin/perl -w

use strict;

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
    # proper.  But TPTP syntax permits there to be multiple fields
    # after the formula proper.  Do we need a TPTP parser?  *sigh*
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

# explicitly negate the conjecture, if there is one
if (defined $conjecture_fof) {
  my $conjecture_formula = extract_formula_from_fof ($conjecture_fof);
  if ($conjecture_formula eq '') {
    warn "Unable to extract the formula from the conjecture fof!";
  } else {
    $conjecture = '~ ( ' . $conjecture_formula . ')';
  }
}

my @non_conjecture_formulas = map { extract_formula_from_fof($_); } @non_conjecture_fofs;
my @non_conjecture_names = map { extract_name_from_fof($_); } @non_conjecture_fofs;

foreach my $i (1 .. scalar @non_conjecture_formulas) {
  my $non_conjecture_fof = $non_conjecture_formulas[$i-1];
  my $non_conjecture_name = $non_conjecture_names[$i-1];
  open (AD_HOC_THEORY, '>', "splork")
    or die "Can't write to splork";
  foreach my $j (1 .. scalar @non_conjecture_formulas) {
    unless ($i == $j) {
      print AD_HOC_THEORY ('fof(' . "ax$j" . ',axiom,' . $non_conjecture_formulas[$j-1] . ').' . "\n");
    }
    if (defined $conjecture) {
      print AD_HOC_THEORY ('fof(our_conjecture,axiom,' . $conjecture . ').' . "\n");
    }
  }
  close AD_HOC_THEORY
    or die "Can't close the output filehandle for splork";
  print $non_conjecture_name, ': ';
  my $mace4_status = system ("tptp_to_ladr < splork | mace4 -p 0 -n 2 -S 1 -s 5 > /dev/null 2>&1");
  my $mace4_exit_code = $mace4_status >> 8;
  if ($mace4_exit_code == 0) {
    print 'needed';
  } else {
    print 'possibly not needed';
  }
  print "\n";
}

system ('rm -f splork');

exit 0;
