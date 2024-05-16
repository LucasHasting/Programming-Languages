print 'Enter your first name: '
first_name = gets.chomp

print 'Enter your age: '
age = gets.chomp.to_i

age *= 12

print first_name + 'is ' + age.to_s + ' months old'
