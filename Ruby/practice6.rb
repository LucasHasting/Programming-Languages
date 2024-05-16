# Name: Lucas Hasting
# Date: 3/6/2024
# Description: Calculate meal tip recommendations

# Resources: Class notes, and Replit autocomplete
# https://www.honeybadger.io/blog/how-to-exit-a-ruby-program/

# set yes_or_no to yes
yes_or_no = 'Y'

while yes_or_no == 'Y'
  # get meal price
  print 'Enter the price of a meal: '
  meal_price = gets.chomp.to_f

  # check if meal price is valid
  if meal_price < 0
    puts 'Invalid meal price'
    yes_or_no = 'N'
    exit(1)
  end

  # display info
  print "10% tip is $#{(meal_price * 0.1).round(2)}\n"
  print "The total with the 10% tip is $#{(meal_price * 1.1).round(2)}\n"
  print "15% tip is $#{(meal_price * 0.15).round(2)}\n"
  print "The total with the 15% tip is $#{(meal_price * 1.15).round(2)}\n"
  print "20% tip is $#{(meal_price * 0.2).round(2)}\n"
  print "The total with the 20% tip is $#{(meal_price * 1.2).round(2)}\n\n"

  # ask the user to go again
  print 'Would you like to go again? (Y/N): '
  yes_or_no = gets.chomp.upcase

  # validate input
  while yes_or_no != 'Y' && yes_or_no != 'N'
    print 'Would you like to go again? (Y/N): '
    yes_or_no = gets.chomp.upcase
  end

  # tell the user bye
  puts 'Have a nice day' if yes_or_no == 'N'
end
