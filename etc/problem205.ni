
!! NOT CURRENTLY WORKING !!

"Project Euler Problem #205 - Dice Game" by "Mercerenies"

The Casino is a room. "A casino full of bright lights and dice of different shapes."

Pyramidal Peter is a person in the Casino. Understand "Peter" as  Pyramidal Peter.

Cubic Colin is a person in the Casino. Understand "Colin" as Cubic Colin.

Playing dice is an action applying to nothing. Understand "play dice" as playing dice.

Total outcomes is a number variable. Total outcomes is 12230590464.

Peter's win set is a list of numbers that varies.

Dice value is a number that varies.

Peter's win count is a real number that varies.

When play begins:
	say total outcomes to 10 decimal places in decimal notation;
	repeat with index running from 1 to 36:
		add 0 to Peter's win set.

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
	say Peter's win set;
	now Peter's win count is 0.0;
	repeat with aa running from 1 to 6:
		repeat with bb running from 1 to 6:
			repeat with cc running from 1 to 6:
				repeat with dd running from 1 to 6:
					repeat with ee running from 1 to 6:
						repeat with ff running from 1 to 6:
							now dice value is aa + bb + cc + dd + ee + ff;
							now Peter's win count is Peter's win count + entry dice value in Peter's win set;
	say Peter's win count to 10 decimal places in decimal notation;
	say Peter's win count / total outcomes to 7 decimal places in decimal notation.

Test me with "play dice".