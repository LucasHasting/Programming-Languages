#!/usr/bin/perl
use IO::Handle;
pipe(I1,O1);
pipe(I2,O2);
O1->autoflush(1);
O2->autoflush(1);

$r1=fork();
if ($r1 == 0)
 {                               # Child
    print O1 "dog\n"; 
    $c2 = <I2>; 
    print STDOUT "Child: $c2\n";
    exit; 
 }
else
 {                              # Parent 
    $d1 = <I1>; 
    print O2 "horse\n";
    print STDOUT "Parent: $d1\n"; 
 }

