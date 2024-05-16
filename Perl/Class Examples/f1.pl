#!/usr/bin/perl

print "PARENT: start\n";
$ret=fork();
if ($ret == 0)
{
 sleep(rand(5));
  print "CHILD: i am the child\n";
  print "CHILD: My id is $$\n";
  $parid = getppid();
  print "CHILD: My parent id is $parid\n";
}
else
{
  sleep(10+rand(5));
  print "PARENT: i am the parent - the childs id is $ret\n";
  print "PARENT: My id is $$\n";
}
