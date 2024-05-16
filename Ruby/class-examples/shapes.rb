#!/usr/local/bin/ruby

def perimSquare(side)
  side*4
end

def areaSquare(side)
  side*side
end

def perimTriangle(side1,side2,side3)
  side1+side2+side3
end

def areaTriangle(base,height)
  base*height/2.0
end

puts perimSquare(5)
puts areaSquare(5)
puts perimTriangle(4,5,6)
puts areaTriangle(5,7)
