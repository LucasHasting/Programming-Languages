#!/usr/bin/perl

@result = `ls -l`;

$size = @result;
print "The size of the list is $size\n"; 

print "0: $result[0]\n";
print "1: $result[1]\n";
print "2: $result[2]\n";
print "3: $result[3]\n";

