#!/usr/bin/perl -w

use strict;

use POSIX qw(floor ceil);

sub copy_string {
  my $string = shift;
  my $num_copies = shift;
  my $final_string = '';
  foreach my $i (1 .. $num_copies) {
    $final_string .= $string;
  }
  return $final_string;
}

if (scalar @ARGV == 0) {
  print 'Usage: summarize-minimization.pl REPROVE-DIRECTORY', "\n";
  exit 1;
}

my $reprove_dir = $ARGV[0];

if (! -e $reprove_dir) {
  print 'Error: the supplied reprove directory ', $reprove_dir, ' does not exist.', "\n";
  exit 1;
}

if (! -d $reprove_dir) {
  print 'Error: the supplied reprove directory ', $reprove_dir, ' is not actually a directory.', "\n";
  exit 1;
}

######################################################################
## Sanity checking
######################################################################

# The directories of our provers exist and contain at least one proof each

my $vampire_proof_dir="$reprove_dir/vampire";
my $eprover_proof_dir="$reprove_dir/eprover";
my $prover9_proof_dir="$reprove_dir/prover9";

# Existence

if (! -e $vampire_proof_dir) {
  print 'Error: the required vampire subdirectory of ', $reprove_dir, ' is missing.', "\n";
  exit 1;
}

if (! -e $eprover_proof_dir) {
  print 'Error: the required eprover subdirectory of ', $reprove_dir, ' is missing.', "\n";
  exit 1;
}

if (! -e $prover9_proof_dir) {
  print 'Error: the required prover9 subdirectory of ', $reprove_dir, ' is missing.', "\n";
  exit 1;
}

# We actually have directories

if (! -d $vampire_proof_dir) {
  print 'Error: the vampire subdirectory of ', $reprove_dir, ' is not actually a directory.', "\n";
  exit 1;
}

if (! -d $eprover_proof_dir) {
  print 'Error: the eprover subdirectory of ', $reprove_dir, ' is not actually a directory.', "\n";
  exit 1;
}

if (! -d $prover9_proof_dir) {
  print 'Error: the prover9 subdirectory of ', $reprove_dir, ' is not actually a directory.', "\n";
  exit 1;
}

# Each prover subdirectory has at least one proof in it

my $num_vampire_proofs = `find "$vampire_proof_dir" -type f -name "*.proof" | wc --lines`;
my $num_eprover_proofs = `find "$eprover_proof_dir" -type f -name "*.proof" | wc --lines`;
my $num_prover9_proofs = `find "$prover9_proof_dir" -type f -name "*.proof" | wc --lines`;

chomp $num_vampire_proofs;
chomp $num_eprover_proofs;
chomp $num_eprover_proofs;

if ($num_vampire_proofs eq 0) {
  print 'Error: we found no proofs in the vampire subdirectory of ', $reprove_dir, '.', "\n";
  exit 1;
}

if ($num_eprover_proofs eq 0) {
  print 'Error: we found no proofs in the eprover subdirectory of ', $reprove_dir, '.', "\n";
  exit 1;
}

if ($num_prover9_proofs eq 0) {
  print 'Error: we found no proofs in the prover9 subdirectory of ', $reprove_dir, '.', "\n";
  exit 1;
}

# Check that we have the "used-principles" file for the final proof of
# each prover.

my $num_vampire_used_principles_files = `find "$vampire_proof_dir" -type f -name "*.used-principles" | wc --lines`;
my $num_eprover_used_principles_files = `find "$eprover_proof_dir" -type f -name "*.used-principles" | wc --lines`;
my $num_prover9_used_principles_files = `find "$prover9_proof_dir" -type f -name "*.used-principles" | wc --lines`;

chomp $num_vampire_used_principles_files;
chomp $num_eprover_used_principles_files;
chomp $num_prover9_used_principles_files;

# Sanity check: the number of used-principles files is equal to the number of proofs

unless ($num_vampire_used_principles_files == $num_vampire_proofs) {
  print 'Error: the number of used-principles files in the vampire subdirectory (', $num_vampire_used_principles_files, ') differs from the number of vampire proofs (', $num_vampire_proofs, ')', "\n";
  exit 1;
}

unless ($num_eprover_used_principles_files == $num_eprover_proofs) {
  print 'Error: the number of used-principles files in the eprover subdirectory (', $num_eprover_used_principles_files, ') differs from the number of eprover proofs (', $num_eprover_proofs, ')', "\n";
  exit 1;
}

unless ($num_prover9_used_principles_files == $num_prover9_proofs) {
  print 'Error: the number of used-principles files in the prover9 subdirectory (', $num_prover9_used_principles_files, ') differs from the number of prover9 proofs (', $num_prover9_proofs, ')', "\n";
  exit 1;
}

my $final_vampire_used_principle_file = `find "$vampire_proof_dir" -type f -name "*.used-principles" | xargs ls -t | head -n 1`;
my $final_eprover_used_principle_file = `find "$eprover_proof_dir" -type f -name "*.used-principles" | xargs ls -t | head -n 1`;
my $final_prover9_used_principle_file = `find "$prover9_proof_dir" -type f -name "*.used-principles" | xargs ls -t | head -n 1`;

chomp $final_vampire_used_principle_file;
chomp $final_eprover_used_principle_file;
chomp $final_prover9_used_principle_file;

# More sanity checks: we actually got a real file

if (-z $final_vampire_used_principle_file) {
  print 'Error: something went wrong finding the final vampire used-principles file.', "\n";
  exit 1;
}

if (-z $final_eprover_used_principle_file) {
  print 'Error: something went wrong finding the final eprover used-principles file.', "\n";
  exit 1;
}

if (-z $final_prover9_used_principle_file) {
  print 'Error: something went wrong finding the final prover9 used-principles file.', "\n";
  exit 1;
}

if (! -e $final_vampire_used_principle_file) {
  print 'Error: we think that ', $final_vampire_used_principle_file, ' is the final used-premises file for vampire, but that file somehow does not exist.';
  exit 1;
}

if (! -e $final_eprover_used_principle_file) {
  print 'Error: we think that ', $final_eprover_used_principle_file, ' is the final used-premises file for eprover, but that file somehow does not exist.';
  exit 1;
}

if (! -e $final_prover9_used_principle_file) {
  print 'Error: we think that ', $final_prover9_used_principle_file, ' is the final used-premises file for prover9, but that file somehow does not exist.';
  exit 1;
}

# Now harvest the the final used-principles files

my @vampire_final_principles = `cat "$final_vampire_used_principle_file" | sort -u | uniq`;
my @eprover_final_principles = `cat "$final_eprover_used_principle_file" | sort -u | uniq`;
my @prover9_final_principles = `cat "$final_prover9_used_principle_file" | sort -u | uniq`;

chomp @vampire_final_principles;
chomp @eprover_final_principles;
chomp @prover9_final_principles;

# For efficiency, make a hash table for each of the principle lists

my %vampire_final_principles_table = ();

foreach my $principle (@vampire_final_principles) {
  $vampire_final_principles_table{$principle} = 0;
}

my %eprover_final_principles_table = ();

foreach my $principle (@eprover_final_principles) {
  $eprover_final_principles_table{$principle} = 0;
}

my %prover9_final_principles_table = ();

foreach my $principle (@prover9_final_principles) {
  $prover9_final_principles_table{$principle} = 0;
}

# Mash them all together

my @all_used_principles = `cat $final_vampire_used_principle_file $final_eprover_used_principle_file $final_prover9_used_principle_file | sort -u | uniq`;
chomp @all_used_principles;

# Now emit a table summarizing things

# Find the longest principle name

my $length_of_longest_principle = 0;

foreach my $principle (@all_used_principles) {
  my $len = length $principle;
  if ($len > $length_of_longest_principle) {
    $length_of_longest_principle = $len;
  }
}

my $padding = abs (length ('Principle') - $length_of_longest_principle);
my $half_padding = floor ($padding / 2);
my $odd = $half_padding == ceil ($padding / 2) ? 1 : 0;

print copy_string (' ', $half_padding), 'Principle', copy_string (' ', $half_padding), ' | vampire | eprover | prover9 |', "\n";
print copy_string ('=', $padding + length ('Principle') + $odd),                           '|=========|=========|=========|', "\n";

foreach my $principle (@all_used_principles) {
  my $principle_length = length $principle;
  my $vampire_marking = defined $vampire_final_principles_table{$principle} ? 'x' : ' ';
  my $eprover_marking = defined $eprover_final_principles_table{$principle} ? 'x' : ' ';
  my $prover9_marking = defined $prover9_final_principles_table{$principle} ? 'x' : ' ';
  my $padding_for_this_principle = $length_of_longest_principle - $principle_length;
  print $principle, copy_string (' ', $padding_for_this_principle), ' |    ', $vampire_marking, '    |    ', $eprover_marking, '    |    ', $prover9_marking, , '    |', "\n";
}

# counts

print copy_string ('=', $padding + length ('Principle') + 1),                           '|=========|=========|=========|', "\n";

$padding = abs (length ('Counts') - $length_of_longest_principle);
$half_padding = floor ($padding / 2);
$odd = $half_padding == ceil ($padding / 2) ? 0 : 1;

print copy_string (' ', $half_padding), 'Counts', copy_string (' ', $half_padding + $odd), ' |    ', scalar @vampire_final_principles, '    |    ', scalar @eprover_final_principles, '    |    ', scalar @prover9_final_principles, '    |    ', "\n";
