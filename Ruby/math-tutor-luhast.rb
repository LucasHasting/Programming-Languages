#!/usr/local/bin/ruby

'''
Name: Lucas Hasting
Date: 3/18/2024
Description: Program that works as an elementary math tutor

Resources: Class Notes, Past Assignments, Files provided by Dr. Terwilliger.
           https://www.geeksforgeeks.org/ruby-numeric-round-function/
'''

#main function to be executed when the program begins
def main
    #create random object and display menu
    ran = Random.new()
    menu(ran)
end

#function to handel the menu of the program
def menu(ran)
    #initilize variables 
    count = 0
    total = 0
    choose = -1

    #choose a difficulty
    diff = choose_difficulty

    #display menu and validate user input
    while (choose != 0)
        choose = -1
        while (choose < 0 || choose > 5)
            print("\nPractice Addition       (1)\n")
            print("Practice Subtraction    (2)\n")
            print("Practice Multiplication (3)\n")
            print("Practice Division       (4)\n")
            print("Change Difficulty       (5)\n")
            print("Quit                    (0)\n")
            print("Enter: ")
            choose = gets.chomp.to_i
        end
        
        puts()

        if (choose > 0 && choose < 5)
            total += 1
        end

        #selction option based on menu
        vals = select_option(choose, ran, diff, count, total)
        
        #update variables
        diff = vals[0]
        count = vals[1]
    end
end

#function to select an option the user picks in the menu
def select_option(choose, ran, diff, count, total)
    if (choose == 1)
        count += addition(ran, diff)
    elsif (choose == 2)
        count += subtraction(ran, diff)
    elsif (choose == 3)
        count += multiplication(ran, diff)
    elsif (choose == 4)
        count += division(ran, diff)
    elsif (choose == 5)
        diff = choose_difficulty
    else
        quit(total, count)
    end
    
    #return values from the options
    vals = Array.new
    vals << diff
    vals << count

    vals
    
end

#function to generate random numbers depending on the difficulty of the problem
def generate_random(ran, diff)
    rand_nums = Array.new
    if (diff == "E")
        rand_nums << ran.rand(0..10)
        rand_nums << ran.rand(0..10)
    end
    
    if (diff == "M")
        rand_nums << ran.rand(0.0..10.0).round(2)
        rand_nums << ran.rand(0.0..10.0).round(2)
    end
    
    if (diff == "D")
        rand_nums << ran.rand(0.0..100.0).round(2)
        rand_nums << ran.rand(0.0..100.0).round(2)
    end
    
    rand_nums
end

#function to ask the user to choose a difficulty
def choose_difficulty
    diff = ""
    while (diff != "E" && diff != "M" && diff != "D")
        print("What difficulty of problems do you want\n")
        print("(E) Easy (M) Medium (D) Difficult\n")
        print("Enter: ")
        diff = gets.chomp
    end
    
    diff
end

#function to print out a message when the user gave their answer
def check_correct(num1, num2)
    if (num1 == num2)
        print("That is the correct answer, great job!\n")
        return(1)
    else
        print("Sorry, that is the wrong answer. The correct answer is: #{num1}\n")
        return(0)
    end
end

#function to handle addition problems
def addition(ran, diff)
    #get random numbers
    rand_nums = generate_random(ran, diff)

    #get the result
    val = rand_nums[0] + rand_nums[1]

    #display the problem and get input from user
    print("#{rand_nums[0]} + #{rand_nums[1]} = ")
    value = gets.chomp.to_f

    #check if it is correct
    check_correct(val, value)
end

#function to handel subtraction problems
def subtraction(ran, diff)
    #get random numbers
    rand_nums = generate_random(ran, diff)
    
    #get the result
    val = rand_nums[0] - rand_nums[1]
    
    #display the problem and get input from user
    print("#{rand_nums[0]} - #{rand_nums[1]} = ")
    value = gets.chomp.to_f
    
    #check if it is correct
    check_correct(val, value)
end

#function to handel multiplication problems
def multiplication(ran, diff)
    #get random numbers
    rand_nums = generate_random(ran, diff)
    
    #get the result
    val = rand_nums[0] * rand_nums[1]
    
    #display the problem and get input from user
    print("#{rand_nums[0]} * #{rand_nums[1]} = ")
    value = gets.chomp.to_f
    
    #check if it is correct
    check_correct(val, value)
end

#function to handel division problems
def division(ran, diff)
    #get random numbers
    rand_nums = generate_random(ran, diff)

    #check if 0 is in the denomonator and if so, make it 1
    if (rand_nums[1] == 0)
        rand_nums[1] = 1
    end

    #get the result
    val = rand_nums[0].to_f / rand_nums[1].to_f
    
    #display the problem and get input from user
    print("#{rand_nums[0]} / #{rand_nums[1]} = ")
    value = gets.chomp.to_f
    
    #check if it is correct
    check_correct(val, value)
end

#function to display some stats when the user enters the quit option
def quit(problem_amount, correct_amount)
    print("Results------------------------------------------\n")
    print("Problems Correct: #{correct_amount}\n")
    print("Problems Attempted: #{problem_amount}\n")
    if (problem_amount == 0)
        print("Percentage Correct: 0%\n")
    else
        print("Percentage Correct: #{((correct_amount.to_f/problem_amount.to_f) * 100).round(2)}%\n")
    end
end


main
