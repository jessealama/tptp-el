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
  my @candidates = `find "$dir" -type f -name "*.used-principles" | xargs ls -t`;
  chomp @candidates;
  my $num_candidates = scalar @candidates;
  my $i = 0;
  my $last_one = undef;
  while (not (defined $last_one) && $i < $num_candidates) {
    my $candidate = $candidates[$i];
    if (-z $candidate) {
      $i++;
    } else {
      $last_one = $candidate;
    }
  }
  if ($i != 0 and defined $last_one) {
    print STDERR ("Warning: the most recent proof file in $dir is empty, and hence unusual;\nWe will cull premises from $last_one as the final successful minimization.\n")
  }
  return $last_one;
}

sub principles_of_proof {
  my $proof = shift;
  my @principles = `cat "$proof" | sort -u | uniq`;
  chomp @principles;
  return \@principles;
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
  # unless ($num_used_principles_files == $num_proofs) {
  #   print 'Error: the number of used-principles files in the ', $prover, ' subdirectory (', $num_used_principles_files, ') differs from the number of proofs (', $num_proofs, ')', "\n";
  #   exit 1;
  # }
}

my %principles_for_prover = ();

foreach my $prover (@provers) {
  my $proof_dir = subdir_for_prover ($prover);
  my %final_principles_table = ();
  my $last_proof = last_successful_proof_in_dir ($proof_dir);
  if (defined $last_proof) {
    my @final_principles = @{principles_of_proof ($last_proof)};
    foreach my $principle (@final_principles) {
      $final_principles_table{$principle} = 0;
    }
    $principles_for_prover{$prover} = \%final_principles_table;
  } else {
    print STDERR ('Warning: we were unable to find any successful proof for ' . $prover . "\n");
    $principles_for_prover{$prover} = undef;
  }
}

sub all_used_principles {
  my %all_used_principles = ();
  foreach my $prover (@provers) {
    my $prover_dir = subdir_for_prover ($prover);
    my $last_proof = last_successful_proof_in_dir ($prover_dir);
    if (defined $last_proof) {
      my @principles = @{principles_of_proof ($last_proof)};
      foreach my $principle (@principles) {
	$all_used_principles{$principle} = 0;
      }
    }
  }
  my @principles = keys %all_used_principles;
  return \@principles;
}

my @all_principles = @{all_used_principles ()};

# Now emit a table summarizing things

# Find the longest principle name

my $length_of_longest_principle = 0;

foreach my $principle (@all_principles) {
  my $len = length $principle;
  if ($len > $length_of_longest_principle) {
    $length_of_longest_principle = $len;
  }
}

my $padding = abs (length ('Principle') - $length_of_longest_principle);

print 'Principle', copy_string (' ', $padding + 1),                       '| vampire | eprover | prover9 |', "\n";
print copy_string ('=', $padding + length ('Principle') + 1),             '|=========|=========|=========|', "\n";

sub summary_line_for_principle {
  my $principle = shift;
  my $padding = shift;

  my $line = "$principle" . copy_string (' ', $padding) . ' |';
  foreach my $prover (@provers) {
    my $marking;
    if (defined ($principles_for_prover{$prover})) {
      $marking = defined $principles_for_prover{$prover}->{$principle} ? 'x' : ' ';
    } else {
      $marking = '-';
    }
    $line .= "    $marking    |";
  }
  return $line;
}

sub counts_line {
  my $line = 'Counts' . copy_string (' ', $padding) . ' |';
  foreach my $prover (@provers) {
    if (defined $principles_for_prover{$prover}) {
      my %principles_table = %{$principles_for_prover{$prover}};
      my @principles = keys %principles_table;
      my $num_principles = scalar @principles;
      $line .= "    " . $num_principles . "    |";
    } else {
      $line .= "    " .       "-"       . "    |";
    }
  }
  return $line;
}

foreach my $principle (@all_principles) {
  my $principle_length = length $principle;
  my $padding_for_this_principle = $length_of_longest_principle - $principle_length;
  my $summary_line = summary_line_for_principle ($principle, $padding_for_this_principle);
  print $summary_line, "\n";
}

# counts

print copy_string ('=', $padding + length ('Principle') + 1), '|=========|=========|=========|', "\n";

$padding = abs (length ('Counts') - $length_of_longest_principle);

print counts_line (), "\n";
