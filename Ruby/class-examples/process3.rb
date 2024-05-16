#!/usr/local/bin/ruby
require 'csv'
students = CSV.read('students.txt')
donald = students.find { |stdnt| stdnt[0] =~ /Donald/ }
donald[0] = "Daffy Duck"
donald[1] = "GOLFING"
CSV.open('students2.txt','w') do |csv|
    students.each do |stdnt|
        csv << stdnt
    end
end

