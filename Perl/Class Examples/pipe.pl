#!/usr/bin/perl

pipe (INPUT, OUTPUT);
$retval = fork();
if ($retval == 0)
 {
   # This is the child  
   close (OUTPUT);
   $line = <INPUT>;
   print("$$ - CHILD: ", $line);
   close(INPUT);
  }
else
 {
   # This is the parent 
   close (INPUT);
   print("$$ - PARENT: Enter a line of input: ");
   $line = <STDIN>;
   print OUTPUT $line;
   close (OUTPUT);
 }
