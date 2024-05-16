#!/usr/bin/perl

for ($i=1; $i<4; $i++)
{
  $x = fork();
  if ($x == 0)
   {
    # this is the child
     print("$$: hi\n");
     exit;
   }
  else
   {
    # this is the parent 
    print ("$$: bye\n");
   }
}


