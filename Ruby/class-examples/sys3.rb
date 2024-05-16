#!/usr/local/bin/ruby

def child
    sleep 2
    puts $$.to_s + " Child"
end

def parent
    sleep 3
    puts $$.to_s + " Parent"
end

f = fork

if f.nil?
    child
else
    parent
end


