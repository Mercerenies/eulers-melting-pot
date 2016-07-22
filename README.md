# Euler's Melting Pot

This is a Project Euler attempt. The catch is that every challenge will be completed in a different programming language.

## Languages Used

Full details of the various solutions are listed below, but for the sake of easy reference, here is an alphabetized
list of the languages that have been used in these challenges so far.
* ///
* 05AB1E
* APL
* Batch
* Befunge
* Brainf**k
* CJam
* Csh
* Elixir
* Go
* Golfscript
* Hexagony
* Io
* Ioke
* J
* Jelly
* Labyrinth
* Make
* NASM
* Piet
* Prolog
* Pyth
* QBASIC
* sed
* Seriously
* Shakespeare
* Smalltalk
* Whitespace
* Zsh

## Completed Challenges

### Euler 1
* File: problem1.golf
* Language: Golfscript
* Compiler / Interpreter: [copy.sh](https://copy.sh/golfscript/)
* Notes: (None)

### Euler 2
* File: problem2.bf
* Language: Brainf**k
* Compiler / Interpreter: [copy.sh](https://copy.sh/brainfuck/)
* Notes: This code was written with the help of a Lisp translator program.

### Euler 3
* File: problem3.ijs
* Language: J
* Compiler / Interpreter: [The de facto standard](http://jsoftware.com/)
* Notes: (None)

### Euler 4
* File: problem4.bat
* Language: Batch
* Compiler / Interpreter: Windows CMD
* Notes: This one is really slow. Interestingly, it seems to be not the palindrome check but the multiplication that slows it down.

### Euler 5
* File: problem5.jelly
* Language: Jelly
* Compiler / Interpreter: [tryitonline.net](http://jelly.tryitonline.net/)
* Notes: (None)

### Euler 6
* File: problem6.hxg
* Language: Hexagony
* Compiler / Interpreter: [The reference implementation](https://github.com/m-ender/hexagony)
* Notes: (None)

### Euler 7
* File: problem7.ws
* Language: Whitespace
* Compiler / Interpreter: [tryitonline.net](http://whitespace.tryitonline.net/)
* Notes: This code was written with the help of a Lisp translator program.

### Euler 8
* File: problem8.io
* Language: Io
* Compiler / Interpreter: [The official implementation](http://iolanguage.org/binaries.html)
* Notes: (None)

### Euler 9
* File: problem9.spl
* Language: Shakespeare
* Compiler / Interpreter: [Lingua::Shakespeare](http://search.cpan.org/dist/Lingua-Shakespeare/lib/Lingua/Shakespeare.pod)
* Notes: (None)

### Euler 10
* File: problem10.cjam
* Language: CJam
* Compiler / Interpreter: [tryitonline.net](http://cjam.tryitonline.net/)
* Notes: (None)

### Euler 11
* File: problem11.s
* Language: NASM
* Compiler / Interpreter: NASM + GCC Linker
* Notes: 64-bit architectures, only.

### Euler 12
* File: problem12.ab
* Language: 05AB1E
* Compiler / Interpreter: [tryitonline.net](http://05ab1e.tryitonline.net/)
* Notes: (None)

### Euler 13
* File: problem13.sed
* Language: sed
* Compiler / Interpreter: GNU sed
* Notes: Other sed implementations may not work. Must supply ./files/problem13.txt as input.

### Euler 14
* File: problem14.st
* Language: Smalltalk
* Compiler / Interpreter: [tutorialspoint.com](http://www.tutorialspoint.com/execute_smalltalk_online.php)
* Notes: The interpreter seems to run faster in Google Chrome.

### Euler 15
* File: problem15.pl
* Language: Prolog
* Compiler / Interpreter: SWI Prolog
* Notes: Uses some SWI-specific database features; may not work in other Prolog implementations.

### Euler 16
* File: problem16.lab
* Language: Labyrinth
* Compiler / Interpreter: [tryitonline.net](http://labyrinth.tryitonline.net/)
* Notes: (None)

### Euler 17
* File: problem17.slsh
* Language: ///
* Compiler / Interpreter: [The included Perl script](https://esolangs.org/wiki////)
* Notes: A surprisingly efficient solution, given the choice of language.

### Euler 18
* File: problem18.mk
* Language: Make
* Compiler / Interpreter: GNU Make
* Notes: Other Make implementations may not work.

### Euler 19
* File: problem19.bas
* Language: QBASIC
* Compiler / Interpreter: [repl.it](https://repl.it/Cc8a)
* Notes: (None)

### Euler 20
* File: problem20.pyth
* Language: Pyth
* Compiler / Interpreter: [tryitonline.net](http://pyth.tryitonline.net/)
* Notes: (None)

### Euler 21
* File: problem21.apl
* Language: APL
* Compiler / Interpreter: [tryapl.org](http://tryapl.org/)
* Notes: (None)

### Euler 22
* File: problem22.exs
* Language: Elixir
* Compiler / Interpreter: [The standard compiler](http://elixir-lang.org/install.html)
* Notes: Depends on ./files/p022_names.txt.

## Euler 23
* File: problem23.go
* Language: Go
* Compiler / Interpreter: [The standard compiler](https://golang.org/doc/install)
* Notes: (None)

## Euler 24
* File: problem24.sh
* Language: Csh
* Compiler / Interpreter: Tcsh
* Notes: Uses a rather unusual algorithm to manually generate each digit.

## Euler 25
* File: problem25.png
* Language: Piet
* Compiler / Interpreter: [rpiet](https://github.com/enebo/rpiet/)
* Notes: Piet interpreter must have bignum integers, not fixed size.

## Euler 26
* File: problem26.sh
* Language: Zsh
* Compiler / Interpreter: Zsh
* Notes: (None)

## Euler 27
* File: problem27.ik
* Language: Ioke
* Compiler / Interpreter: [Ioke for the JVM](https://ioke.org/download.html)
* Notes: A fairly slow solution.

## Euler 28
* File: problem28.bf
* Language: Befunge
* Compiler / Interpreter: [quirkster.com](http://www.quirkster.com/iano/js/befunge.html)
* Notes: The immediate shift on the first row is necessary; the first row is used for storage in this program.

## Euler 29
* File: problem29.srs
* Language: Seriously
* Compiler / Interpreter: [tryitonline.net](http://seriously.tryitonline.net/)
* Notes: The seemingly pointless union with the empty list at the end serves to remove duplicates.

## Valid Languages

Mostly, I am just using common sense to determine what constitutes a "language". In particular,
a language does not have to be Turing-complete in order for me to consider using it for this challenge.
Here are just a few of the guidelines I am following. These are in no way permanent or binding and will
change as I progress.
* ~~All Assembly languages are the same. Assembly will appear exactly once total.~~
* BF-derivatives and BF-clones are considered identical to Brainf**k and will not be used.
* Lisp derivatives are considered distinct~~, but Scheme and its derivatives all fall under the umbrella of
  Scheme~~. That is, there will be a Racket, a Common Lisp, and an ELisp, which are all distinct languages.
* Sufficiently different BASIC dialects will be considered distinct. That is, Visual BASIC, Liberty BASIC, and
  QBASIC are distinct languages, but Liberty BASIC and JustBASIC are one and the same.
* The result should be in a normal, human readable form. For example, in BF it is insufficient to print the
  ASCII character associated with the integer result; it is required to print the result as a number. There is
  no specific required format, as some languages may include some garbage around the answer, so long as the
  answer as outputted is clear and readable.

## Final Notes

You'll notice that I'm not using many of the mainstream languages so far. This is not because I'm trying to
make this challenge harder than it is; this is a purely strategic decision. The later Project Euler problems
are more difficult, so I am saving the powerful and easy-to-use languages for them and using obscure, esoteric
languages for the easier problems.
