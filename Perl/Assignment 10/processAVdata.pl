#!/usr/bin/perl
use CGI qw(:standard);

print header;
print start_html("AV Rental Site");

$rate = 0;
$name = param("name");
$date = param("date");
$time = param("time");
$item = param("item");
$hours = param("hours");

print "<p>Hello $name</p>\n";
print "<p>The date is $date</p>\n";
print "<p>The time is $time</p>\n";
print "<p>The hours is $hours</p>\n";

if ($item eq "L")
{
	print "<p>You rented a laptop</p>\n";
	$rate = 3.5 * $hours;
} elsif ($item eq "P"){
	print "<p>You rented a projector</p>\n";
	$rate = 2.75 * $hours;
} elsif ($item eq "B") {
	print "<p>You rented a laptop and a projector</p>\n";
	$rate = 5.25 * $hours;
}

print "<p>Your cost is \$$rate</p>\n";

open (FH, ">>orders.txt");
print FH "$name~$date~$time~$item~$hours\n";
close (FH);

print end_html;
