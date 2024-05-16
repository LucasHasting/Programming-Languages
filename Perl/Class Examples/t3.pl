#!/usr/bin/perl

print "This is before the exec command\n";
exec('ls');
print "Experts say this line will never execute\n";
