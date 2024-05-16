#!/usr/bin/perl

pipe (INPUT, OUTPUT);
$retval = fork();
if ($retval == 0)
 {
   # This is the child  
   close (OUTPUT);
   $line = <INPUT>;
   print("CHILD: Hello from child\n");
   printf("CHILD: I got my ID from my parent: %d\n", $line);
   print "CHILD: I already knew my ID was $$ by using \$\$\n";
   close(INPUT);
  }
else
 {
   # This is the parent 
   close (INPUT);
   $line = $retval;
   print "PARENT: I will send the child its ID in 5 seconds\n";
   sleep(5);        # parent is tired, must take a short nap
   print OUTPUT $line;
   close (OUTPUT);
 }
