#!/usr/local/bin/ruby

class Square
  def initialize
    if defined?(@@numSquares)
      @@numSquares += 1
    else
      @@numSquares = 1
    end
  end

  def self.count
    @@numSquares
  end
end


a=Square.new
b=Square.new

puts Square.count
