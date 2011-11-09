#!/usr/bin/perl -w

use strict;

sub copy_string {
  my $string = shift;
  my $num_copies = shift;
  my $final_string = '';
  foreach my $i (1 .. $num_copies) {
    $final_string .= $string;
  }
  return $final_string;
}

sub last_successful_proof_in_dir {
  my $dir = shift;
  my $last = `find "$dir" -type f -name "*.used-principles" | xargs ls -t | head -n 1`;
  chomp $last;
  return $last;
}

sub count_proofs_in_dir {
  my $dir = shift;
  my $count = `find "$dir" -type f -name "*.proof" | wc --lines`;
  chomp $count;
  return $count;
}

sub count_used_principles_in_dir {
  my $dir = shift;
  my $count = `find "$dir" -type f -name "*.used-principles" | wc --lines`;
  chomp $count;
  return $count;
}

if (scalar @ARGV == 0) {
  print 'Usage: summarize-minimization.pl REPROVE-DIRECTORY', "\n";
  exit 1;
}

my $reprove_dir = $ARGV[0];

sub subdir_for_prover {
  my $prover = shift;
  chomp $prover;
  return "$reprove_dir/$prover";
}

my @provers = ('vampire', 'eprover', 'prover9');

######################################################################
## Sanity checking
######################################################################

if (! -e $reprove_dir) {
  print 'Error: the supplied reprove directory ', $reprove_dir, ' does not exist.', "\n";
  exit 1;
}

if (! -d $reprove_dir) {
  print 'Error: the supplied reprove directory ', $reprove_dir, ' is not actually a directory.', "\n";
  exit 1;
}

# The directories of our provers exist and contain at least one proof each

foreach my $prover (@provers) {
  my $prover_dir = subdir_for_prover ($prover);
  if (! -e $prover_dir) {
    print 'Error: the required ', $prover, ' subdirectory of ', $reprove_dir, ' is missing.', "\n";
    exit 1;
  }
  if (! -d $prover_dir) {
    print 'Error: the ', $prover, ' subdirectory of ', $reprove_dir, ' is not actually a directory.', "\n";
    exit 1;
  }
  my $num_proofs = count_proofs_in_dir ($prover_dir);
  if ($num_proofs == 0) {
    print 'Error: we found no proofs in the ', $prover, '  subdirectory of ', $reprove_dir, '.', "\n";
    exit 1;
  }
  my $num_used_principles_files = count_used_principles_in_dir ($prover_dir);
  unless ($num_used_principles_files == $num_proofs) {
    print 'Error: the number of used-principles files in the ', $prover, ' subdirectory (', $num_used_principles_files, ') differs from the number of proofs (', $num_proofs, ')', "\n";
    exit 1;
  }
}

my $vampire_proof_dir="$reprove_dir/vampire";
my $eprover_proof_dir="$reprove_dir/eprover";
my $prover9_proof_dir="$reprove_dir/prover9";

my $final_vampire_used_principle_file = last_successful_proof_in_dir ($vampire_proof_dir);
my $final_eprover_used_principle_file = last_successful_proof_in_dir ($eprover_proof_dir);
my $final_prover9_used_principle_file = last_successful_proof_in_dir ($prover9_proof_dir);

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

print 'Principle', copy_string (' ', $padding + 1),                       '| vampire | eprover | prover9 |', "\n";
print copy_string ('=', $padding + length ('Principle') + 1),             '|=========|=========|=========|', "\n";

foreach my $principle (@all_used_principles) {
  my $principle_length = length $principle;
  my $vampire_marking = defined $vampire_final_principles_table{$principle} ? 'x' : ' ';
  my $eprover_marking = defined $eprover_final_principles_table{$principle} ? 'x' : ' ';
  my $prover9_marking = defined $prover9_final_principles_table{$principle} ? 'x' : ' ';
  my $padding_for_this_principle = $length_of_longest_principle - $principle_length;
  print $principle, copy_string (' ', $padding_for_this_principle), ' |    ', $vampire_marking, '    |    ', $eprover_marking, '    |    ', $prover9_marking, , '    |', "\n";
}

# counts

print copy_string ('=', $padding + length ('Principle') + 1), '|=========|=========|=========|', "\n";

$padding = abs (length ('Counts') - $length_of_longest_principle);

print 'Counts', copy_string (' ', $padding), ' |    ', scalar @vampire_final_principles, '    |    ', scalar @eprover_final_principles, '    |    ', scalar @prover9_final_principles, '    |    ', "\n";
