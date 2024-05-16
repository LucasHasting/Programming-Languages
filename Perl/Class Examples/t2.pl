#!/usr/bin/perl

print "Enter string to search for: ";
$str = <STDIN>;
chomp($str);
system("grep $str *");
