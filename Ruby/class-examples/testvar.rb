#!/usr/local/bin/ruby

def test
    w = 10  #w is a local varible 
    puts w
end

test

def test2
    puts $t
end

$t = 5      #$t is a global variable
test2
