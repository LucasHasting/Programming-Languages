#!/usr/bin/perl
#Name: Lucas Hasting
#Description: Computes tax information for the state of Alabama
#Course: CS 410W
#Date: 2/12/2024
#Instructor: Mark Terwilliger

#display welcome message
print "This program calculates tax information based on income tax for the state of
Alabama \nfor single residents with a total income of \$6000 or more.\n\n";

#loop until the user ends the program
do {
    #get user first and last name
    print "first name: ";
    $first_name = <STDIN>;
    chomp($first_name);
    print "last name: ";
    $last_name = <STDIN>;
    chomp($last_name);

    #set deductions
    $deductions = 1500;
    print "The deduction is set to $deductions\n";
    print "\nAll inputs must be whole numbers\n\n";

    #get input from user
    print "Alabama state tax witheld from your W2: \$";
    $tax_withheld = <STDIN>;
    chomp($state_tax);
    print "Total income from W2: \$";
    $total_income = <STDIN>;
    chomp($total_income);

    #make calculations
    $taxable_income = $total_income - $deductions;
    print "\nThe taxable income is \$$taxable_income\n";
    $income_tax_due = 110 + (0.05 * ($taxable_income - 3000));
    print "The income tax due \$$income_tax_due\n";
    $refund = $tax_withheld - $income_tax_due;
    print "The refund amount is \$$refund\n";

    #display how much is owed to the state of alabama
    if ($refund < 0)
    {
        $refund = -$refund;
        print "You owe the state of Alabama \$$refund\n";
    }

    #display thank you message
    print "\nThank you $first_name $last_name for using this program\n";

    #ask the user to go again
    print "Would you like repeat the process for another customer (Y/N)? ";
    $reply = <STDIN>;
    chomp($reply);
    if ($reply eq "Y")
    {
        print "\n";
    }
} while ($reply eq "Y");