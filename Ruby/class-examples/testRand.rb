#!/usr/local/bin/ruby

num = Random.new()

puts(num.rand())
puts
puts(num.rand(10.0))
puts(num.rand(5.0..10.0)) # number in [5, 10]
puts(num.rand(5.0...10.0)) # number in [5, 10)
puts
puts num.rand(10) # number is integer [0, 10)
puts num.rand(5..10) # number is integer [5, 10]
puts num.rand(5...10) # number is integer [5, 10)
