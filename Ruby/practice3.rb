print 'Enter score #1: '
score1 = gets.chomp.to_f

print 'Enter score #2: '
score2 = gets.chomp.to_f

print 'Enter score #3: '
score3 = gets.chomp.to_f

average = (score1 + score2 + score3) / 3
print "The average is #{average.round(2)}\n"
