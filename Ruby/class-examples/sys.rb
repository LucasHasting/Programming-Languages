#!/usr/local/bin/ruby

c = system('date')
puts c

d = `date`
puts "The date is " + d

exec "./hello.rb"

puts "the final exam cancelled"
