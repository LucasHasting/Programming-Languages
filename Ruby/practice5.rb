#Name: Lucas Hasting
#Date: 3/6/2024
#Description: Print all powers of a number < 1000000

#Resources: Class notes, and Replit autocomplete

#get the base
print 'What is the base? '
base = gets.chomp.to_i

#declare variables
result = 0
count = 0

#get powers and display them
while result * base < 1000000
  result = base**count
  print "#{base} to the power of #{count} is #{result}\n"
  count += 1
end