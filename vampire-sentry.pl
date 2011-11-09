#!/usr/bin/perl -w

# Copy standard input to standard output until we encounter an input
# line that looks like
#
#   'Termination reason: Satisfiable'
#
# at which point we exit uncleanly.  If we never encounter such a
# line, terminate cleanly.

use strict;

# launch epclextract

open (ECHO, '|echo')
  or (print STDERR 'Error: we are unable to open a pipe to echo.' && exit 1);

while (defined (my $line = <STDIN>)) {
  if ($line =~ /Termination reason: Satisfiable/) {
    close ECHO
      or (print STDERR 'Error: our problem is countersatisfiable, so we wish to close our pipe to echo, but we cannot.' && exit 1);
    exit 2;
  } else {
    print ECHO ($line);
  }
}

close ECHO
  or (print STDERR 'Error: we are unable to close the pipe to echo.' && exit 1);

exit 0;
