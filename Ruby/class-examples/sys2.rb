#!/usr/local/bin/ruby

f = fork

if f.nil?
    puts "child"
else
    puts "parent"
end


