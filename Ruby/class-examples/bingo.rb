#!/usr/local/bin/ruby

num = Random.new()
moreGames = true

while moreGames do

picked = Array.new
totals = Hash.new


done = "n"
while done != "y" do 
    ball = num.rand(1..75)
    if (!(picked.include?(ball)))
        print "B-" if ball.between?(1, 15)
        print "I-" if ball.between?(16, 30)
        print "N-" if ball.between?(31, 45)
        print "G-" if ball.between?(45, 60)
        print "O-" if ball.between?(61, 75)
        puts(ball)
    elsif(picked.length != 75)
        break
    else
        next
    end

    print("winner (y/n)? ")
    done = gets.chomp
end
    print("Who won? ")
    name = gets.chomp

    if totals.key?(name)
        totals[name] += 1
    else
        totals[name] = 1
    end

    print("more games (y/n)? ")
    done = gets.chomp
    if (done == "n")
        moreGames = false
    end
end

totals.each do |key, value|
    puts "#{key} has #{value} win(s)"
end
