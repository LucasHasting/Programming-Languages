#!/usr/local/bin/ruby
require 'pstore'

'''
Name: Lucas Hasting
Date: 3/11/2024
Description: Program that allows users to create restaurant menus and can display them.
             It uses persistent objects to store and load the objects.

Resources: Class Notes, Past Assignments, Files provided by Dr. Terwilliger.
           https://apidock.com/ruby/Object/instance_variable_get
           https://www.geeksforgeeks.org/ruby-array-length-function/
'''

#main function for the program
def main
    #create persistent object
    store = PStore.new("menuFile")

    #declare menus array
    menus = Array.new

    #display welcome message
    print("WELCOME TO THE RESTERAUNT MENU CREATION SYSTEM\n")
    
    #load from persistent object
    menus = load_object(store, menus)

    #show the main menu
    main_menu(menus)

    #save persistent object
    save_object(store, menus)
end

#function that provides the main menu for the program
def main_menu(menus)
    #initilize default values
    yes_or_no = "Y"
    value = -1

    #loop for validitiy
    while(yes_or_no == "Y")
        #loop for validitiy
        while(value < 0 || value > 2)
            #display the menu, and get input from the user
            print("\n")
            puts("Select one of the following: ")
            puts("Add Menu: 1")
            puts("Display Menu: 2")
            puts("Quit: 0")
            print("Enter: ")
            value = gets.chomp.to_i
            print("\n")
        end

        #reset yes_or_no
        yes_or_no = ""

        #select option from main menu
        if (value == 0)
            yes_or_no = "N"
        else
            #loop for validitiy
            while (yes_or_no != 'Y' && yes_or_no != 'N')
                yes_or_no = main_select_option(value, menus)
                #reset value
                value = 3
            end
        end
    end
end

#function display the items of a menu based on its number
def display_items(menus, menu_number)
    menus[menu_number].display_items
end

#function to select an option from the main menu
def main_select_option(value, menus)
    if (value == 1)
        create_menu(menus)
    elsif (value == 2)
        display_menu(menus)
    end

    #ask the user if if they want to continue
    if (menus.length == 0)
        print("Do you want to go back to the main menu (Y/N)? ")
    else    
        print("\nDo you want to go back to the main menu (Y/N)? ")
    end

    value = gets.chomp.upcase
end

#function to create a new menu and add it to the menus list
def create_menu(menus)
    #create new menu
    menu = Menu.new

    #menu for menu creation, shows output and gets input from user to create menu
    print("What is the name of the menu? ")
    menu_name = gets.chomp
    menu.set_name(menu_name)

    print("How many items do you want to add? ")
    items_amount = gets.chomp.to_i
    print("\n")
    
    for i in 1..(items_amount)
        print("What is the item? ")
        item_name = gets.chomp.to_s
        
        print("How much does it cost? $")
        item_cost = gets.chomp.to_f
        
        menu.add_item(item_name, item_cost)
    end

    #add the menu to the array of menus
    menus << menu
end

#function to display the items in a menu
def display_menu(menus)
    #do nothing if there are no menus
    if (menus.length == 0)
        return -1
    end

    #choose from menu
    val = choose_menu(menus)

    #display the menus items
    display_items(menus, val)
end    

#function to allow the user to choose from a list of menus
def choose_menu(menus)
    #initilize default values
    value = -1
    i = 1

    #loop for validitiy
    while(value <= 0 || value > (i-1))
        #reset variables
        value = -1
        i = 1

        #display the menus 
        for menu in menus
            name = menu.instance_variable_get(:@name)
            print("Menu #{i}: #{name}\n")
            i += 1
        end

        #ask the user for a menu number
        print("Enter a menu number: ")
        value = gets.chomp.to_i
        print("\n")
    end

    #return the index of the menu
    (value - 1)
end

#function to load the menu objects from a persistent object
def load_object(store, menus)
    #load persistent objects
    store.transaction do
        val = store[:menu]
        if (val != nil)
            menus = val
        end
    end

    #copy items to object
    for menu in menus
        #get attributes
        item_list = menu.instance_variable_get(:@item_list)
        cost_list = menu.instance_variable_get(:@cost_list)
        item_amount = menu.instance_variable_get(:@item_amount)
        name = menu.instance_variable_get(:@name)

        #set attributes
        menu = Menu.new
        menu.copy(item_list, cost_list, item_amount, name)
    end

    #return menus array
    menus
end

#function to save all menu objects in a persistent object
def save_object(store, menus)
    #store every index of menus into the store perisistent object
    store.transaction do
        store[:menu] = Array.new
        for menu in menus do 
            store[:menu] << menu
        end
    end

    #display finished message
    print("Finished saving menu contents!\n")
end

#menu class used in program
class Menu
    #constructor initilizes default values
    def initialize
        @item_list = []
        @cost_list = []
        @item_amount = 0
        @name = ""
    end

    #method set the menu's name
    def set_name(name)
        @name = name
    end

    #method to add an item to the menu
    def add_item(item, cost)
        @item_list << item
        @cost_list << cost
        @item_amount += 1
    end

    #method to display all items on the menu
    def display_items
        #if there are no items return -1 for error
        if (@item_amount == 0)
            return -1
        end

        #if there are items, display them
        for i in 0..(@item_amount - 1) do
            print("Item \##{i+1} is #{@item_list[i].to_s} at $#{@cost_list[i].to_s} \n")
        end
    end

    #method to copy all menu items to the current object
    def copy(items, costs, item_amount, name)
        @item_amount = item_amount
        @item_list = items
        @cost_list = costs
        @name = name
    end
    
end

#call the main function to start the program
main
