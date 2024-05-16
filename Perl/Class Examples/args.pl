#!/usr/bin/perl

die("Usage: argv.pl length width") if (@ARGV != 2);
 
print "1st argument is $ARGV[0]\n";
print "2nd argument is $ARGV[1]\n";

$length = $ARGV[0];
$width  = $ARGV[1]; 

$area = $length * $width;

print "$length x $width is $area\n";
