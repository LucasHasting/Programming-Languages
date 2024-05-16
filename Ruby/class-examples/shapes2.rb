#!/usr/local/bin/ruby

class Shape
end

class Square < Shape
  def initialize(side)
    @side = side           # @side is an instance variable
  end                      # associated with a single square object
                           # can be accessed by any square method
  def area
    @side * @side
  end
 
  def perimeter
    @side * 4
  end
end
