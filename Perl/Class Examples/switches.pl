#!/usr/bin/perl
# r reverse lines
# l line number
# s sort
# n sort numeric

use Getopt::Std;

if (! getopts('rlsn'))
{    die "Usage: switches.pl -rlsn\n";}
my @file = <>;
if ($opt_s) {    @file = sort @file;}
if ($opt_n) {    @file = sort {$a <=> $b} @file;}
if ($opt_r) {    @file = reverse @file;}

my $i = 1;
foreach my $line (@file)
  { if ($opt_l)
       { print "$i: $line";     $i++;    }
    else
       { print $line;    }
  }



