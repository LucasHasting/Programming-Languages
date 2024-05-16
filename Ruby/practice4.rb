#Name: Lucas Hasting
#Date: 3/6/2024
#Description: Calculate score averages

#Resources: Class notes, and Replit autocomplete

#get scores amount
print 'How many scores? '
scores = gets.chomp.to_i

#get scores and their sum
sum = 0
for i in 1..scores
  print "Enter score ##{i}: "
  score = gets.chomp.to_f
  sum += score
end

#calculate average
average = sum / scores
print "The average is #{average.round(1)}\n"