#!/usr/bin/perl

foreach $i (0..4)
{
  $ids[$i]=fork();
  if ($ids[$i]==0)
    {
      # This is the child process
      &child;
    }
}

# This is the parent process

foreach $i (0..4)
{
  $x = wait;
  print "PARENT: I just got word that $x died\n";
}
  
sub child
{
  sleep (5+rand(25));
  print "Child: $$  Hello I'm done\n";
  exit;
}

