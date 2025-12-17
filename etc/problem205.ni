
"Project Euler Problem #205 - Dice Game" by "Mercerenies"

The Casino is a room. "A casino full of bright lights and dice of different shapes."

Pyramidal Peter is a person in the Casino. Understand "Peter" as Pyramidal Peter.

Cubic Colin is a person in the Casino. Understand "Colin" as Cubic Colin.

Playing dice is an action applying to nothing. Understand "play dice" as playing dice.

Total outcomes is a list of numbers that varies.

Peter's win set is a list of numbers that varies.

Dice value is a number that varies.

Local win count is a number that varies.

Peter's win count is a list of numbers that varies.

Peter's new win count is a list of numbers that varies.

Numerator is a list of numbers that varies.

Quotient is a list of numbers that varies.

Current difference is a list of numbers that varies.

Carry bit is a number that varies.

First digit is a number that varies.

Second digit is a number that varies.

Peter's final wins is a number that varies.

When play begins:
	add 0 to total outcomes;
	add 1 to total outcomes;
	add 0 to total outcomes;
	add 1 to total outcomes;
	add 1 to total outcomes;
	add 0 to total outcomes;
	add 1 to total outcomes;
	add 1 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 1 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	add 0 to total outcomes;
	repeat with index running from 1 to 36:
		add 0 to Peter's win set;
	repeat with index running from 1 to 40:
		add 0 to Peter's win count.

Carry out playing dice:
	repeat with aa running from 1 to 4:
		repeat with bb running from 1 to 4:
			repeat with cc running from 1 to 4:
				repeat with dd running from 1 to 4:
					repeat with ee running from 1 to 4:
						repeat with ff running from 1 to 4:
							repeat with gg running from 1 to 4:
								repeat with hh running from 1 to 4:
									repeat with ii running from 1 to 4:
										now dice value is aa + bb + cc + dd + ee + ff + gg + hh + ii;
										repeat with index running from 6 to dice value - 1:
											now entry index in Peter's win set is entry index in Peter's win set + 1;
	repeat with aa running from 1 to 6:
		repeat with bb running from 1 to 6:
			repeat with cc running from 1 to 6:
				repeat with dd running from 1 to 6:
					repeat with ee running from 1 to 6:
						repeat with ff running from 1 to 6:
							now dice value is aa + bb + cc + dd + ee + ff;
							now local win count is entry dice value in Peter's win set;
							change numerator to have 0 entries;
							repeat with index running from 1 to 35:
								add remainder after dividing local win count by 2 to numerator;
								now local win count is local win count / 2;
							reverse numerator;
							change quotient to have 0 entries;
							repeat with index running from 1 to 40:
								remove entry 1 from numerator;
								add 0 to numerator;
								change current difference to have 0 entries;
								now carry bit is 1;
								repeat with jndex running from 1 to 35:
									now first digit is entry 36 - jndex in numerator;
									now second digit is 1 - entry 36 - jndex in total outcomes;
									add remainder after dividing first digit + second digit + carry bit by 2 to current difference;
									now carry bit is (first digit + second digit + carry bit) / 2;
								reverse current difference;
								if entry 1 of current difference is 0:
									add 1 to quotient;
									now numerator is current difference;
								otherwise:
									add 0 to quotient;
							change Peter's new win count to have 0 entries;
							now carry bit is 0;
							repeat with index running from 1 to 40:
								now first digit is entry 41 - index of Peter's win count;
								now second digit is entry 41 - index of quotient;
								add remainder after dividing first digit + second digit + carry bit by 2 to Peter's new win count;
								now carry bit is (first digit + second digit + carry bit) / 2;
							reverse Peter's new win count;
							now Peter's win count is peter's new win count;
	now Peter's final wins is 0;
	now Peter's final wins is Peter's final wins + entry 1 of Peter's win count * 500000000;
	now Peter's final wins is Peter's final wins + entry 2 of Peter's win count * 250000000;
	now Peter's final wins is Peter's final wins + entry 3 of Peter's win count * 125000000;
	now Peter's final wins is Peter's final wins + entry 4 of Peter's win count * 62500000;
	now Peter's final wins is Peter's final wins + entry 5 of Peter's win count * 31250000;
	now Peter's final wins is Peter's final wins + entry 6 of Peter's win count * 15625000;
	now Peter's final wins is Peter's final wins + entry 7 of Peter's win count * 7812500;
	now Peter's final wins is Peter's final wins + entry 8 of Peter's win count * 3906250;
	now Peter's final wins is Peter's final wins + entry 9 of Peter's win count * 1953125;
	now Peter's final wins is Peter's final wins + entry 10 of Peter's win count * 976562;
	now Peter's final wins is Peter's final wins + entry 11 of Peter's win count * 488281;
	now Peter's final wins is Peter's final wins + entry 12 of Peter's win count * 244140;
	now Peter's final wins is Peter's final wins + entry 13 of Peter's win count * 122070;
	now Peter's final wins is Peter's final wins + entry 14 of Peter's win count * 61035;
	now Peter's final wins is Peter's final wins + entry 15 of Peter's win count * 30517;
	now Peter's final wins is Peter's final wins + entry 16 of Peter's win count * 15258;
	now Peter's final wins is Peter's final wins + entry 17 of Peter's win count * 7629;
	now Peter's final wins is Peter's final wins + entry 18 of Peter's win count * 3814;
	now Peter's final wins is Peter's final wins + entry 19 of Peter's win count * 1907;
	now Peter's final wins is Peter's final wins + entry 20 of Peter's win count * 953;
	now Peter's final wins is Peter's final wins + entry 21 of Peter's win count * 476;
	now Peter's final wins is Peter's final wins + entry 22 of Peter's win count * 238;
	now Peter's final wins is Peter's final wins + entry 23 of Peter's win count * 119;
	now Peter's final wins is Peter's final wins + entry 24 of Peter's win count * 59;
	now Peter's final wins is Peter's final wins + entry 25 of Peter's win count * 29;
	now Peter's final wins is Peter's final wins + entry 26 of Peter's win count * 14;
	now Peter's final wins is Peter's final wins + entry 27 of Peter's win count * 7;
	now Peter's final wins is Peter's final wins + entry 28 of Peter's win count * 3;
	now Peter's final wins is Peter's final wins + entry 29 of Peter's win count * 1;
	now Peter's final wins is (Peter's final wins + 50) / 100;
	say "0.";
	say Peter's final wins.


Test me with "play dice".