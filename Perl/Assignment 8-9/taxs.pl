#!/usr/bin/perl
use CGI qw(:standard);

#Name: Lucas Hasting
#Description: Computes tax information for the state of Alabama
#Course: CS 410W
#Date: 2/12/2024
#Instructor: Mark Terwilliger
#References: https://www.thoughtco.com/comparison-operators-compare-values-in-perl-2641145
#            https://www.tutorialspoint.com/perl/perl_printf.htm
#            class notes

#Link:       https://cs.csis.work/~luhast/form.html

#print beggining html
print header;
print start_html("Tax Information");
print "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">";
print "<h1>Tax Results</h1>";

#get information
$name = param("name");
$tax_withheld = param("stateIncomeTaxWitheld");
$total_income = param("annualWage");
$status = param("status");
$income_tax = 0;

#get taxable income and income tax
if ($status eq "Single") {
    $deductions = 1500;
    $taxable_income = $total_income - $deductions; 
    if ($taxable_income < 0){
        $taxable_income = 0;
    }
    if ($taxable_income <= 500) {
        print $taxable_income;
        $income_tax = $taxable_income * 0.02;
    } elsif ($taxable_income <= 3000) {
        $income_tax = ($taxable_income-500) * 0.04 + 10;
    } else {
        $income_tax = (($taxable_income-3000) * 0.05) + 110;
    } 
} else {
    $deductions = 3000;
    $taxable_income = $total_income - $deductions; 
    if ($taxable_income < 0){
        $taxable_income = 0;
    }

    if ($taxable_income <= 1000) {
        $income_tax = $taxable_income * 0.02;
    } elsif ($taxable_income <= 6000) {
        $income_tax = ($taxable_income-1000) * 0.04 + 20;
    } else {
        $income_tax = ($taxable_income-6000) * 0.05 + 220;
    }	
}

#print information to website
printf("<p>The deduction is set to \$%.0f.</p>\n", $deductions);
printf("<p>The taxable income is \$%.0f.</p>\n", $taxable_income);
printf("<p>The income tax is \$%.0f.</p>\n", $income_tax);

#get refund and display it
$refund = $tax_withheld - $income_tax;
if ($refund < 0)
{
    $refund = -$refund;
    printf("<p>You owe the state of Alabama \$%.0f.</p>\n", $refund);
} else {
    printf("<p>The refund amount is \$%.0f.</p>\n", $refund);
}

#display thank you message
print "<p>Thank you $name, for using this program.</p>\n";

#return to other websites
print "<p>Go back to the <a href=\"index.html\">main page</a>.</p>";
print "<p>Enter tax information <a href=\"form.html\">again</a>.</p>";

#end the html
print end_html;