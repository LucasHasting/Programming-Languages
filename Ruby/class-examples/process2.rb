#!/usr/local/bin/ruby
require 'csv'
students = CSV.read('students.csv')
puts students[0][0]
puts students[1][0]
puts students[2][0]
puts students[3][0]
puts students[4][0]
donald = students.find { |stdnt| stdnt[0] =~ /Donald/ }
p donald
youngStudents = students.find_all do |stdnt|
    stdnt[3].to_i.between?(18,20)
end
p youngStudents
youngStudents.each do |s|
    puts s[1]
end
