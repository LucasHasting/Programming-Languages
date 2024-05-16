#!/usr/local/bin/ruby
require 'csv'
CSV.open('students.csv').each do |student|
# print student
# puts student
    p student
end
