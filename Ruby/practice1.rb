print 'Enter Number: '
num = gets.chomp.to_i
puts 'Positive' if num > 0
puts 'Negative' if num < 0
puts 'Zero' if num == 0
puts 'Divisbile by 3' if num % 3 == 0
puts 'Divisbile by 5' if num % 5 == 0
puts 'At least a million but less than 2 million' if num >= 1_000_000 && num < 2_000_000
puts 'The number of digits in the number is ' + num.to_s.length.to_s
puts 'The second digit is ' + num.to_s[1] + "\n" if num.to_s.length >= 2
puts 'The number is not 2 digits long' if num.to_s.length < 2
