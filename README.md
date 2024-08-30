# Euler's Melting Pot

This is a Project Euler attempt. The catch is that every challenge will be completed in a different
programming language.

Euler's Melting Pot is also being run as an educational YouTube series, where I talk about the code I've written here. See https://www.youtube.com/playlist?list=PL1ltQrTP1szcCAD-Jm-6eQdV_zhnm3oNq

## Languages Used

Full details of the various solutions are listed below, but for the sake of easy reference, here is an
alphabetized list of the languages that have been used in these challenges so far.
* \*\>\<\>
* ///
* 05AB1E
* 1.1
* 11l
* 42
* \>\<\>
* Actually
* Ada
* AGSPL
* ALGOL 68
* Alice
* Anti-Array
* APL
* Arc
* ArkScript
* AssemblyScript
* AutoHotkey
* AWK
* Aya
* Batch
* bc
* BeanShell
* Beeswax
* Befalse
* Befreak
* Befunge
* Befunk
* Brainf\*\*k
* Brat
* Broccoli
* Burlesque
* Bussin
* C#
* Chef
* CIL
* CJam
* COBOL
* Coconut
* Comefrom0x10
* COMPLEX
* 🆒
* Csh
* Cubix
* D
* Dafny
* Dhall
* Dictu
* Dip
* Dogescript
* Dry
* Dylan
* Earl Grey
* ed
* Eiffel
* ELisp
* Elixir
* Emoji
* Emoticon
* Emotinomicon
* Enchilada
* Erlang
* 𝔼𝕊𝕄𝕚𝕟
* Factor
* Factorio
* FALSE
* Fennel
* FiM++
* Fish
* Folders
* Forth
* Fortran
* Fourier
* Funciton
* Game Builder Garage
* Genie
* Gibberish
* Glava
* Gleam
* GML
* GNU Octave
* Go
* Golfscript
* Grocery List
* Gwion
* Hanabi
* Hexagony
* Hy
* Hyperscript
* i
* Icon
* IntercalScript
* Io
* Ioke
* J
* Japt
* Jasmin
* Javagony
* Javagrid
* Jelly
* Joy
* JSF\*\*k
* JustBASIC
* K
* Kitten
* Labyrinth
* LaTeX
* Lean
* LilyPond
* LLVM IR
* m4
* Make
* MagiStack
* MASM
* Math++
* MATL
* Microsoft Excel
* Minecraft
* Minus
* MontiLang
* MoonScript
* Mouse-2002
* NASM
* naz
* Nemerle
* Nim
* Nit
* Oasis
* Oberon-07
* Objective-C
* OCaml
* Odin
* Oxide
* Pascal
* Parrot IR
* PASM
* Perchance
* Picat
* Pickle
* Piet
* Pike
* Pikt
* Pip
* Pizza
* Pony
* Potassco
* Prolog
* Pyf\*\*ck
* Pyth
* Pyramid Scheme
* QBASIC
* Rebol
* Rockstar
* Roy
* Rust
* Scratch
* sed
* Seriously
* Shakespeare
* Smalltalk
* SML
* SNOBOL
* Snowman
* Stuck
* SuperCollider
* Swift
* Taxi
* Tcl
* Tome
* Tovie
* TRANSCRIPT
* TypeScript
* V
* Vala
* Vale
* VBA
* Verilog
* Visual Basic .NET
* WebAssembly
* Wenyan
* Whirl
* Whitespace
* Wren
* Wyvern
* X10
* XSLT
* Z
* Zig
* zkl
* Zsh

## Completed Challenges

### Euler 1
* File: [problem1.golf](problem1.golf)
* Language: [Golfscript](http://www.golfscript.com/golfscript/)
* Compiler / Interpreter: [copy.sh](https://copy.sh/golfscript/)
* Notes: (None)

### Euler 2
* File: [problem2.bf](problem2.bf)
* Language: [Brainf\*\*k](https://en.wikipedia.org/wiki/Brainfuck)
* Compiler / Interpreter: [copy.sh](https://copy.sh/brainfuck/)
* Notes: This code was written with the help of a [Lisp translator program](etc/bfgen.lisp).

### Euler 3
* File: [problem3.ijs](problem3.ijs)
* Language: [J](http://jsoftware.com/)
* Compiler / Interpreter: [The de facto standard](http://jsoftware.com/)
* Notes: (None)

### Euler 4
* File: [problem4.bat](problem4.bat)
* Language: [Batch](https://en.wikipedia.org/wiki/Batch_file)
* Compiler / Interpreter: Windows CMD
* Notes: This one is really slow. Interestingly, it seems to be not the palindrome check but the multiplication that slows it down.

### Euler 5
* File: [problem5.jelly](problem5.jelly)
* Language: [Jelly](https://github.com/DennisMitchell/jelly)
* Compiler / Interpreter: [TIO Nexus](http://tio.run/#jelly)
* Notes: (None)

### Euler 6
* File: [problem6.hxg](problem6.hxg)
* Language: [Hexagony](https://github.com/m-ender/hexagony)
* Compiler / Interpreter: [The reference implementation](https://github.com/m-ender/hexagony)
* Notes: (None)

### Euler 7
* File: [problem7.ws](problem7.ws)
* Language: [Whitespace](http://compsoc.dur.ac.uk/whitespace/tutorial.html)
* Compiler / Interpreter: [TIO Nexus](http://tio.run/#whitespace)
* Notes: This code was written with the help of a [Lisp translator program](etc/wsgen.lisp).

### Euler 8
* File: [problem8.io](problem8.io)
* Language: [Io](http://iolanguage.org/)
* Compiler / Interpreter: [The official implementation](http://iolanguage.org/binaries.html)
* Notes: (None)

### Euler 9
* File: [problem9.spl](problem9.spl)
* Language: [Shakespeare](http://shakespearelang.sourceforge.net/)
* Compiler / Interpreter: [Lingua::Shakespeare](http://search.cpan.org/dist/Lingua-Shakespeare/lib/Lingua/Shakespeare.pod)
* Notes: (None)

### Euler 10
* File: [problem10.cjam](problem10.cjam)
* Language: [CJam](https://sourceforge.net/projects/cjam/)
* Compiler / Interpreter: [TIO Nexus](http://tio.run/#cjam)
* Notes: (None)

### Euler 11
* File: [problem11.s](problem11.s)
* Language: [NASM](http://www.nasm.us/)
* Compiler / Interpreter: NASM + GCC Linker
* Notes: 64-bit architectures, only. Uses Windows calling convention; to run on Linux, call puts with RDI rather than RCX.

### Euler 12
* File: [problem12.ab](problem12.ab)
* Language: [05AB1E](https://github.com/Adriandmen/05AB1E/)
* Compiler / Interpreter: [TIO Nexus](http://tio.run/#05ab1e)
* Notes: (None)

### Euler 13
* File: [problem13.sed](problem13.sed)
* Language: [sed](http://linux.die.net/man/1/sed)
* Compiler / Interpreter: GNU sed
* Notes: Other sed implementations may not work. Must supply `./files/problem13.txt` as input.

### Euler 14
* File: [problem14.st](problem14.st)
* Language: [Smalltalk](https://en.wikipedia.org/wiki/Smalltalk)
* Compiler / Interpreter: [tutorialspoint.com](http://www.tutorialspoint.com/execute_smalltalk_online.php)
* Notes: The interpreter seems to run faster in Google Chrome.

### Euler 15
* File: [problem15.pl](problem15.pl)
* Language: [Prolog](https://en.wikipedia.org/wiki/Prolog)
* Compiler / Interpreter: SWI Prolog
* Notes: Uses some SWI-specific database features; may not work in other Prolog implementations.

### Euler 16
* File: [problem16.lab](problem16.lab)
* Language: [Labyrinth](https://github.com/m-ender/labyrinth)
* Compiler / Interpreter: [TIO Nexus](http://tio.run/#labyrinth)
* Notes: (None)

### Euler 17
* File: [problem17.slsh](problem17.slsh)
* Language: [///](https://esolangs.org/wiki////)
* Compiler / Interpreter: [The included Perl script](https://esolangs.org/wiki////)
* Notes: A surprisingly efficient solution, given the choice of language.

### Euler 18
* File: [problem18.mk](problem18.mk)
* Language: [Make](https://www.gnu.org/software/make/)
* Compiler / Interpreter: GNU Make
* Notes: Other Make implementations may not work.

### Euler 19
* File: [problem19.bas](problem19.bas)
* Language: [QBASIC](https://en.wikipedia.org/wiki/QBasic)
* Compiler / Interpreter: [repl.it](https://repl.it/languages/qbasic)
* Notes: (None)

### Euler 20
* File: [problem20.pyth](problem20.pyth)
* Language: [Pyth](https://github.com/isaacg1/pyth)
* Compiler / Interpreter: [TIO Nexus](http://TIO.run/#pyth)
* Notes: (None)

### Euler 21
* File: [problem21.apl](problem21.apl)
* Language: [APL](https://en.wikipedia.org/wiki/APL_(programming_language))
* Compiler / Interpreter: [tryapl.org](http://tryapl.org/)
* Notes: (None)

### Euler 22
* File: [problem22.exs](problem22.exs)
* Language: [Elixir](http://elixir-lang.org/)
* Compiler / Interpreter: [The standard compiler](http://elixir-lang.org/install.html)
* Notes: Depends on `./files/p022_names.txt`.

### Euler 23
* File: [problem23.go](problem23.go)
* Language: [Go](https://golang.org/)
* Compiler / Interpreter: [The standard compiler](https://golang.org/doc/install)
* Notes: (None)

### Euler 24
* File: [problem24.sh](problem24.sh)
* Language: [Csh](https://en.wikipedia.org/wiki/C_shell)
* Compiler / Interpreter: Tcsh
* Notes: Uses a rather unusual algorithm to manually generate each digit.

### Euler 25
* File: [problem25.png](problem25.png)
* Language: [Piet](http://www.dangermouse.net/esoteric/piet.html)
* Compiler / Interpreter: [rpiet](https://github.com/enebo/rpiet/)
* Notes: Piet interpreter must have bignum integers, not fixed size.

### Euler 26
* File: [problem26.sh](problem26.sh)
* Language: [Zsh](https://en.wikipedia.org/wiki/Z_shell)
* Compiler / Interpreter: Zsh
* Notes: (None)

### Euler 27
* File: [problem27.ik](problem27.ik)
* Language: [Ioke](https://ioke.org/)
* Compiler / Interpreter: [Ioke for the JVM](https://ioke.org/download.html)
* Notes: A somewhat slow solution.

### Euler 28
* File: [problem28.bf](problem28.bf)
* Language: [Befunge](https://esolangs.org/wiki/Befunge)
* Compiler / Interpreter: [quirkster.com](http://www.quirkster.com/iano/js/befunge.html)
* Notes: The immediate shift on the first row is necessary; the first row is used for storage in this program.

### Euler 29
* File: [problem29.srs](problem29.srs)
* Language: [Seriously](https://github.com/Mego/Seriously/tree/v1)
* Compiler / Interpreter: [TIO Nexus](http://tio.run/#seriously)
* Notes: The seemingly pointless union with the empty list at the end serves to remove duplicates.

### Euler 30
* File: [problem30.stk](problem30.stk)
* Language: [Stuck](https://github.com/kade-robertson/stuck)
* Compiler / Interpreter: [The official interpreter](https://github.com/kade-robertson/stuck)
* Notes: The interpreter seems to require some minor modification to work with Unicode on some systems, a somewhat slow solution.

### Euler 31
* File: [problem31.sml](problem31.sml)
* Language: [SML](https://en.wikipedia.org/wiki/Standard_ML)
* Compiler / Interpreter: [tutorialspoint.com](http://www.tutorialspoint.com/execute_smlnj_online.php)
* Notes: (None)

### Euler 32
* File: [problem32.cbl](problem32.cbl)
* Language: [COBOL](https://en.wikipedia.org/wiki/COBOL)
* Compiler / Interpreter: [tutorialspoint.com](http://www.tutorialspoint.com/compile_cobol_online.php)
* Notes: (None)

### Euler 33
* File: [problem33.xsl](problem33.xsl)
* Language: [XSLT](http://www.w3schools.com/xsl/)
* Compiler / Interpreter: [Mozilla Firefox](https://www.mozilla.org/en-US/firefox/new/)
* Notes: Load `./files/p33.xml` to run the XSLT code; requires disabling `security.fileuri.strict_origin_policy` on Firefox due to local system security constraints.

### Euler 34
* File: [problem34.f95](problem34.f95)
* Language: [Fortran](https://en.wikipedia.org/wiki/Fortran)
* Compiler / Interpreter: [tutorialspoint.com](http://www.tutorialspoint.com/compile_fortran_online.php)
* Notes: (None)

### Euler 35
* File: [problem35.ml](problem35.ml)
* Language [OCaml](http://www.ocaml.org/)
* Compiler / Interpreter: [OCaml on Windows](http://protz.github.io/ocaml-installer/)
* Notes: (None)

### Euler 36
* File: [problem36.fth](problem36.fth)
* Language: [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language))
* Compiler / Interpreter: [repl.it](https://repl.it/languages/forth)
* Notes: (None)

### Euler 37
* File: [problem37.jsf](problem37.jsf)
* Language: [JSF\*\*k](http://www.jsfuck.com/)
* Compiler / Interpreter: [The provided runner](http://www.jsfuck.com/)
* Notes: Code was generated with the help of the [official website's translator](https://jsfuck.com/).

### Euler 38
* File: [problem38.bas](problem38.bas)
* Language: [JustBASIC](http://justbasic.com/)
* Compiler / Interpreter: [The provided IDE](http://justbasic.com/download.html)
* Notes: (None)

### Euler 39
* File: [problem39.adb](problem39.adb)
* Language: [Ada](https://en.wikipedia.org/wiki/Ada_(programming_language))
* Compiler / Interpreter: [tutorialspoint.com](https://www.tutorialspoint.com/compile_ada_online.php)
* Notes: (None)

### Euler 40
* File: [problem40.tex](problem40.tex)
* Language: [LaTeX](https://en.wikipedia.org/wiki/LaTeX)
* Compiler / Interpreter: [MiKTeX](https://miktex.org/)
* Notes: May have to increase the memory limit, depending on OS.

### Euler 41
* File: [problem41.asm](problem41.asm)
* Language: [MASM](https://en.wikipedia.org/wiki/Microsoft_Macro_Assembler)
* Compiler / Interpreter: [MASM32](http://www.masm32.com/)
* Notes: A somewhat slow solution.

### Euler 42
* File: [problem42.j](problem42.j)
* Language: [Jasmin](http://jasmin.sourceforge.net/about.html)
* Compiler / Interpreter: [The standard assembler](http://jasmin.sourceforge.net/)
* Notes: Depends on `./files/p042_words.txt`.

### Euler 43
* File: [problem43.tcl](problem43.tcl)
* Language: [Tcl](http://wiki.tcl.tk/299)
* Compiler / Interpreter: [The standard interpreter](http://tcl.tk/)
* Notes: (None)

### Euler 44
* File: [problem44.xlsm](problem44.xlsm)
* Language: [VBA](https://en.wikipedia.org/wiki/Visual_Basic_for_Applications)
* Compiler / Interpreter: [Microsoft Excel 2013](https://products.office.com/en-us/microsoft-excel-2013)
* Notes: Run the macro `Sheet1.Euler44`; code is available at [etc/problem44.bas](etc/problem44.bas).

### Euler 45
* File: [problem45.fish](problem45.fish)
* Language: [><>](https://esolangs.org/wiki/Fish)
* Compiler / Interpreter: [fish.py](https://gist.github.com/anonymous/6392418)
* Notes: (None)

### Euler 46
* File: [problem46.matl](problem46.matl)
* Language: [MATL](https://esolangs.org/wiki/MATL)
* Compiler / Interpreter: [The official interpreter](https://github.com/lmendo/MATL/)
* Notes: (None)

### Euler 47
* File: [problem47.sno](problem47.sno)
* Language: [SNOBOL](http://www.snobol4.org/)
* Compiler / Interpreter: [CSNOBOL4](http://www.snobol4.org/csnobol4/curr/)
* Notes: A very slow solution.

### Euler 48
* File: [problem48.sb2](problem48.sb2)
* Language: [Scratch](https://scratch.mit.edu/)
* Compiler / Interpreter: [The project editor](https://scratch.mit.edu/projects/editor/)
* Notes: Screenshot is available at [etc/Scratch/problem48.png](etc/Scratch/problem48.png).

### Euler 49
* File: [problem49.il](problem49.il)
* Language: [CIL](https://en.wikipedia.org/wiki/Common_Intermediate_Language)
* Compiler / Interpreter: [Mono](http://www.mono-project.com/)
* Notes: (None)

### Euler 50
* File: [problem50.alg](problem50.alg)
* Language: [ALGOL 68](https://en.wikipedia.org/wiki/ALGOL_68)
* Compiler / Interpreter: [tutorialspoint.com](https://www.tutorialspoint.com/execute_algol_online.php)
* Notes: (None)

### Euler 51
* File: [problem51.d](problem51.d)
* Language: [D](https://dlang.org/)
* Compiler / Interpreter: [DMD](https://dlang.org/download.html)
* Notes: (None)

### Euler 52
* File: [problem52.wren](problem52.wren)
* Language: [Wren](http://wren.io/)
* Compiler / Interpreter: [The reference implementation](https://github.com/munificent/wren)
* Notes: (None)

### Euler 53
* File: [problem53.sfish](problem53.sfish)
* Language: [*><>](http://esolangs.org/wiki/Starfish)
* Compiler / Interpreter: [The official interpreter](https://github.com/redstarcoder/go-starfish/releases)
* Notes: (None)

### Euler 54
* File: [problem54.rs](problem54.rs)
* Language: [Rust](https://www.rust-lang.org/)
* Compiler / Interpreter: [The standard compiler](https://www.rust-lang.org/en-US/install.html)
* Notes: (None)

### Euler 55
* File: [problem55.pas](problem55.pas)
* Language: [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language))
* Compiler / Interpreter: [tutorialspoint.com](https://www.tutorialspoint.com/compile_pascal_online.php)
* Notes: (None)

### Euler 56
* File: [problem56.bc](problem56.bc)
* Language: [bc](https://en.wikipedia.org/wiki/Bc_(programming_language))
* Compiler / Interpreter: GNU bc
* Notes: (None)

### Euler 57
* File: [problem57.factor](problem57.factor)
* Language: [Factor](http://factorcode.org/)
* Compiler / Interpreter: [The official interpreter](http://factorcode.org/#downloads)
* Notes: (None)

### Euler 58
* File: [problem58.el](problem58.el)
* Language: [ELisp](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
* Compiler / Interpreter: [GNU Emacs](https://www.gnu.org/software/emacs/)
* Notes: A somewhat slow solution.

### Euler 59
* File: [problem59.pike](problem59.pike)
* Language: [Pike](http://pike.lysator.liu.se/)
* Compiler / Interpreter: [The official interpreter](http://pike.lysator.liu.se/download/)
* Notes: (None)

### Euler 60
* File: [problem60.gml](problem60.gml)
* Language: [Game Maker Language](https://docs.yoyogames.com/source/dadiospice/002_reference/001_gml%20language%20overview/)
* Compiler / Interpreter: [GMLive](https://yal.cc/r/gml/)
* Notes: The interpreter seems to run faster in Google Chrome.

### Euler 61
* File: [problem61.ily](problem61.ily)
* Language: [LilyPond](http://lilypond.org/website/index.html)
* Compiler / Interpreter: [The official interpreter](http://lilypond.org/website/download.html)
* Notes: Outputs result to PDF.

### Euler 62
* File: [problem62.roy](problem62.roy)
* Language: [Roy](http://roy.brianmckenna.org/)
* Compiler / Interpreter: [The provided compiler](http://roy.brianmckenna.org/)
* Notes: May have to increase maximum stack size to run.

### Euler 63
* File: [problem63.ench](problem63.ench)
* Language: [Enchilada](http://www.enchiladacode.nl/)
* Compiler / Interpreter: [The official interpreter](http://www.enchiladacode.nl/download.html)
* Notes: This code was compiled with the help of a [Perl translator program](etc/enchgen.pl).

### Euler 64
* File: [problem64.m4](problem64.m4)
* Language: [m4](https://en.wikipedia.org/wiki/M4_(computer_language))
* Compiler / Interpreter: [GNU m4](https://www.gnu.org/software/m4/m4.html)
* Notes: Other m4 implementations may not work.

### Euler 65
* File: [problem65.fnc](problem65.fnc)
* Language: [Funciton](https://esolangs.org/wiki/Funciton)
* Compiler / Interpreter: [The official interpreter](https://github.com/Timwi/Funciton)
* Notes: A somewhat slow solution.

### Euler 66
* File: [problem66.bean](problem66.bean)
* Language: [BeanShell](https://beanshell.github.io/)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#beanshell)
* Notes: (None)

### Euler 67
* File: [problem67.gl](problem67.gl)
* Language: [Grocery List](https://esolangs.org/wiki/Grocery_List)
* Compiler / Interpreter: [Archived interpreter](https://web.archive.org/web/20160404194506/https://bpaste.net/show/50974d4c80f8)
* Notes: Code was written with the help of a [Lisp translator program](etc/glgen.lisp). Pipe `./files/p067_triangle.txt` as standard input. File must have Unix line endings.

### Euler 68
* File: [problem68.ll](problem68.ll)
* Language: [LLVM IR](https://llvm.org/docs/LangRef.html)
* Compiler / Interpreter: [The official compiler](http://releases.llvm.org/download.html#6.0.0)
* Notes: (None)

### Euler 69
* File: [problem69.m](problem69.m)
* Language: [Objective-C](https://en.wikipedia.org/wiki/Objective-C)
* Compiler / Interpreter: GCC
* Notes: (None)

### Euler 70
* File: [problem70.Mod](problem70.Mod)
* Language: [Oberon-07](http://oberon07.com/)
* Compiler / Interpreter: [OBNC](https://miasap.se/obnc/)
* Notes: A somewhat slow solution.

### Euler 71
* File: [problem71.taxi](problem71.taxi)
* Language: [Taxi](https://bigzaphod.github.io/Taxi/)
* Compiler / Interpreter: [The provided interpreter](https://bigzaphod.github.io/Taxi/taxi.cpp)
* Notes: A very slow solution.

### Euler 72
* File: [problem72.nim](problem72.nim)
* Language: [Nim](https://nim-lang.org/)
* Compiler / Interpreter: [The official compiler](https://nim-lang.org/install.html)
* Notes: (None)

### Euler 73
* File: [problem73.cool](problem73.cool)
* Language: [🆒](https://esolangs.org/wiki/%F0%9F%86%92)
* Compiler / Interpreter: [The official interpreter](https://gitlab.com/fogity/squared-cool)
* Notes: A very slow solution. Screenshot is available at [etc/problem73_screenshot.png](etc/problem73_screenshot.png).

### Euler 74
* File: [problem74.pasm](problem74.pasm)
* Language: [PASM](https://en.wikipedia.org/wiki/Parrot_assembly_language)
* Compiler / Interpreter: [The Parrot VM](https://github.com/parrot/parrot)
* Notes: (None)

### Euler 75
* File: [problem75.mou](problem75.mou)
* Language: [Mouse-2002](http://mouse.davidgsimpson.com/mouse2002/index.html)
* Compiler / Interpreter: [The provided interpreter](http://mouse.davidgsimpson.com/mouse2002/mouse_c.txt)
* Notes: Interpreter requires a bit of tweaking to work with modern compilers. Will need to increase the `ARRAYSIZE` to run.

### Euler 76
* File: [problem76.befalse](problem76.befalse)
* Language: [Befalse](https://esolangs.org/wiki/Befalse)
* Compiler / Interpreter: [quirkster.com](http://www.quirkster.com/iano/js/befalse.html)
* Notes: Scroll down; the first 202 lines are used for storage.

### Euler 77
* File: [problem77.false](problem77.false)
* Language: [FALSE](https://esolangs.org/wiki/FALSE)
* Compiler / Interpreter: [quirkster.com](http://www.quirkster.com/iano/js/false-js.html)
* Notes: Lightly commented code is available at [etc/problem77_commented.false](etc/problem77_commented.false).

### Euler 78
* File: [problem78.vala](problem78.vala)
* Language: [Vala](https://wiki.gnome.org/Projects/Vala)
* Compiler / Interpreter: [valac](https://wiki.gnome.org/Projects/Vala/ValaPlatforms)
* Notes: (None)

### Euler 79
* File: [problem79.ed](problem79.ed)
* Language: [ed](https://www.gnu.org/software/ed/manual/ed_manual.html)
* Compiler / Interpreter: [GNU ed 1.18](https://www.gnu.org/software/ed/)
* Notes: Supply `./files/0079_keylog.txt` as argument and `problem79.ed` as piped standard input.

### Euler 80
* File: [problem80.wyv](problem80.wyv)
* Language: [Wyvern](https://wyvernlang.github.io/)
* Compiler / Interpreter: [The provided interpreter](https://github.com/wyvernlang/wyvern)
* Notes: (None)

### Euler 81
* File: [problem81.v](problem81.v)
* Language: [V](https://esolangs.org/wiki/V_(DJMcMayhem))
* Compiler / Interpreter: [The provided interpreter](https://github.com/DJMcMayhem/V)
* Notes: Supply `./files/p081_matrix.txt` as input; the source file contains control characters that may or may not render correctly in various text editors. Screenshot is available at [etc/problem81_screenshot.png](etc/problem81_screenshot.png).

### Euler 82
* File: [problem82.awk](problem82.awk)
* Language: [AWK](https://en.wikipedia.org/wiki/AWK)
* Compiler / Interpreter: [mawk](https://invisible-island.net/mawk/)
* Notes: Pass `./files/p082_matrix.txt` as input.

### Euler 83
* File: [problem83.djs](problem83.djs)
* Language: [Dogescript](https://dogescript.io/)
* Compiler / Interpreter: [The provided compiler](https://github.com/dogescript/dogescript/)
* Notes: (None)

### Euler 84
* File: [problem84.e](problem84.e)
* Language: [Eiffel](https://www.eiffel.org/)
* Compiler / Interpreter: [The provided compiler](https://www.eiffel.org/downloads)
* Notes: `./etc/build84.sh` may be helpful in compiling the source.

### Euler 85
* File: [problem85.cub](problem85.cub)
* Language: [Cubix](https://esolangs.org/wiki/Cubix)
* Compiler / Interpreter: [The provided interpreter](http://ethproductions.github.io/cubix/)
* Notes: A somewhat slow solution.

### Euler 86
* File: [problem86.aya](problem86.aya)
* Language: [Aya](https://esolangs.org/wiki/Aya)
* Compiler / Interpreter: [The provided interpreter](https://github.com/nick-paul/aya-lang)
* Notes: (None)

### Euler 87
* File: [problem87.brat](problem87.brat)
* Language: [Brat](http://brat-lang.org/)
* Compiler / Interpreter: [The provided interpreter](https://try.brat-lang.org/#)
* Notes: (None)

### Euler 88
* File: [problem88.esmin](problem88.esmin)
* Language: [𝔼𝕊𝕄𝕚𝕟](https://bennyboy.tech/ESMin/)
* Compiler / Interpreter: [The provided interpreter](https://bennyboy.tech/ESMin/interpreter3.html)
* Notes: (None)

### Euler 89
* File: [problem89.11](problem89.11)
* Language: [1.1](https://esolangs.org/wiki/1.1)
* Compiler / Interpreter: [The provided Perl script](https://github.com/Sxakalo/1.1-Lang)
* Notes: Pass `./files/p089_roman.txt` as input.

### Euler 90
* File: [problem90.1+1j](problem90.1+1j)
* Language: [COMPLEX](https://esolangs.org/wiki/COMPLEX)
* Compiler / Interpreter: [The provided interpreter](https://github.com/Taneb/COMPLEX)
* Notes: Code was written with the help of a [Lisp translator program](etc/cmgen.lisp).

### Euler 91
* File: [problem91.aarr](problem91.aarr)
* Language: [Anti-Array](https://esolangs.org/wiki/Anti-Array)
* Compiler / Interpreter: [The provided interpreter](https://github.com/Demonthos/Anti-Array)
* Notes: A somewhat slow solution; the interpreter crashes at the end for unknown reasons, but the answer printed is still correct.

### Euler 92
* File: [problem92.sc](problem92.sc)
* Language: [SuperCollider](https://supercollider.github.io/)
* Compiler / Interpreter: [The official interpreter](https://supercollider.github.io/download)
* Notes: A somewhat slow solution.

### Euler 93
* File: [problem93.gs](problem93.gs)
* Language: [Genie](https://wiki.gnome.org/Projects/Genie)
* Compiler / Interpreter: [valac](https://wiki.gnome.org/Projects/Vala/ValaPlatforms)
* Notes: Ignore the lengthy list of pointer cast warnings when compiling.

### Euler 94
* File: [problem94.fact](problem94.fact)
* Language: [Factorio](https://www.factorio.com/)
* Compiler / Interpreter: [Factorio](https://www.factorio.com/)
* Notes: Import the blueprint, then set the lower-left constant combinator to R=0 to run. Thanks to DaveMcW for his [compact digit display design](https://forums.factorio.com/viewtopic.php?t=19825). Screenshot is available at [etc/problem94_screenshot.png](etc/problem94_screenshot.png).

### Euler 95
* File: [problem95.pir](problem95.pir)
* Language: [Parrot IR](https://en.wikipedia.org/wiki/Parrot_intermediate_representation)
* Compiler / Interpreter: [The Parrot VM](https://github.com/parrot/parrot)
* Notes: (None)

### Euler 96
* File: [problem96.java](problem96.java)
* Language: [Javagony](https://esolangs.org/wiki/Javagony)
* Compiler / Interpreter: [openjdk](https://openjdk.java.net/)
* Notes: An incredibly slow solution.

### Euler 97
* File: [problem97.mpp](problem97.mpp)
* Language: [Math++](https://esolangs.org/wiki/Math%2B%2B)
* Compiler / Interpreter: [The provided interpreter](https://pastebin.com/mnxZk1cz)
* Notes: A very slow solution, outputs its answer in scientific notation but with sufficient precision.

### Euler 98
* File: [problem98.gl](problem98.gl)
* Language: [Glava](https://esolangs.org/wiki/Glava)
* Compiler / Interpreter: [The provided interpreter](https://github.com/ZekNikZ/Glava)
* Notes: (None)

### Euler 99
* File: [problem99.hnb](problem99.hnb)
* Language: [Hanabi](https://esolangs.org/wiki/Hanabi)
* Compiler / Interpreter: [The provided interpreter](https://github.com/elyatai/hanabi/)
* Notes: Pass `./files/p099_base_exp.txt` as input.

### Euler 100
* File: [problem100.snow](problem100.snow)
* Language: [Snowman](https://esolangs.org/wiki/Snowman)
* Compiler / Interpreter: [The provided interpreter](https://github.com/tckmn/snowman-lang/)
* Notes: Code was written with the help of a [Lisp translator program](etc/smgen.lisp).

### Euler 101
* File: [problem101.gibb](problem101.gibb)
* Language: [Gibberish](https://esolangs.org/wiki/Gibberish_(programming_language))
* Compiler / Interpreter: [The unofficial Python interpreter](https://esolangs.org/wiki/User:Marinus/Gibberish_interpreter)
* Notes: (None)

### Euler 102
* File: [problem102.magi](problem102.magi)
* Language: [MagiStack](https://esolangs.org/wiki/MagiStack)
* Compiler / Interpreter: [MagiStack 1.2 Interpreter](https://github.com/tripl3dogdare/MagiStack/blob/master/magistack-1.2.py)
* Notes: Code was written with the help of a [Lisp translator program](etc/mggen.lisp); provide `./files/p102_triangles.txt` as input.

### Euler 103
* File: [problem103.dylan](problem103.dylan)
* Language: [Dylan](https://opendylan.org/)
* Compiler / Interpreter: [Open Dylan](https://opendylan.org/download/index.html)
* Notes: `./etc/build103.sh` may be helpful in building the source.

### Euler 104
* File: [problem104.zkl](problem104.zkl)
* Language: [zkl](http://www.zenkinetic.com/zkl.html)
* Compiler / Interpreter: [The official interpreter](http://www.zenkinetic.com/zklDownloads.html)
* Notes: (None)

### Euler 105
* File: [problem105.fnl](problem105.fnl)
* Language: [Fennel](https://fennel-lang.org/)
* Compiler / Interpreter: [fennel-0.7.0.lua](https://fennel-lang.org/downloads/fennel-0.7.0)
* Notes: (None)

### Euler 106
* File: [problem106.joy](problem106.joy)
* Language: [Joy](https://www.latrobe.edu.au/humanities/research/research-projects/past-projects/joy-programming-language)
* Compiler / Interpreter: [Archived interpreter](https://www.latrobe.edu.au/__data/assets/file/0007/240298/Joy-Programming.zip)
* Notes: The interpreter dumps some noisy debugging information, which can be safely ignored.

### Euler 107
* File: [problem107.pi](problem107.pi)
* Language: [Picat](http://picat-lang.org/)
* Compiler / Interpreter: [The official interpreter](http://picat-lang.org/download.html)
* Notes: (None)

### Euler 108
* File: [problem108.agda](problem108.agda)
* Language: [Agda](https://en.wikipedia.org/wiki/Agda_(programming_language))
* Compiler / Interpreter: [Agda 2.6.0.1](https://github.com/agda/agda)
* Notes: Requires [agda-stdlib 1.2](https://github.com/agda/agda-stdlib).

### Euler 109
* File: [problem109.png](problem109.png)
* Language: [Befunk](https://esolangs.org/wiki/Befunk)
* Compiler / Interpreter: [The provided interpreter](https://github.com/TieSoul/Befunk)
* Notes: A very slow solution; code was written with the help of a [Python translator program](etc/bkgen.py).

### Euler 110
* File: [problem110.pip](problem110.pip)
* Language: [Pip](https://esolangs.org/wiki/Pip)
* Compiler / Interpreter: [The provided interpreter](https://github.com/dloscutoff/pip)
* Notes: A somewhat slow solution.

### Euler 111
* File: [problem111.n](problem111.n)
* Language: [Nemerle](http://nemerle.org/About)
* Compiler / Interpreter: [The official compiler](https://github.com/rsdn/nemerle)
* Notes: (None)

### Euler 112
* File: [problem112.fimpp](problem112.fimpp)
* Language: [FiM++](https://esolangs.org/wiki/FiM%2B%2B)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#fimpp)
* Notes: A somewhat slow solution.

### Euler 113
* File: [problem113.chef](problem113.chef)
* Language: [Chef](https://www.dangermouse.net/esoteric/chef.html)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#chef)
* Notes: Code was written with the help of a [Perl translator program](etc/chgen.lisp).

### Euler 114
* File: [problem114.bees](problem114.bees)
* Language: [Beeswax](https://esolangs.org/wiki/Beeswax)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#beeswax)
* Notes: Code was written with the help of a [Lisp translator program](etc/bwgen.lisp); a somewhat slow solution.

### Euler 115
* File: [problem115.mt](problem115.mt)
* Language: [MontiLang](https://rosettacode.org/wiki/Category:MontiLang)
* Compiler / Interpreter: [The provided interpreter](https://github.com/lduck11007/MontiLang)
* Notes: A somewhat slow solution.

### Euler 116
* File: [problem116.ktn](problem116.ktn)
* Language: [Kitten](https://kittenlang.org/)
* Compiler / Interpreter: [The provided interpreter](https://github.com/evincarofautumn/kitten)
* Notes: (None)

### Euler 117
* File: [problem117.pony](problem117.pony)
* Language: [Pony](https://www.ponylang.io/)
* Compiler / Interpreter: [Pony Playground](https://playground.ponylang.io/)
* Notes: (None)

### Euler 118
* File: [problem118.jl](problem118.jl)
* Language: [Julia](https://julialang.org/)
* Compiler / Interpreter: [Julia 1.2.0](https://julialang.org/downloads/)
* Notes: (None)

### Euler 119
* File: [problem119.gleam](problem119.gleam)
* Language: [Gleam](https://gleam.run/)
* Compiler / Interpreter: [The official compiler](https://github.com/gleam-lang/gleam/releases)
* Notes: `./etc/build119.sh` may be helpful in building the source.

### Euler 120
* File: [problem120.tome](problem120.tome)
* Language: [Tome](https://esolangs.org/wiki/Tome)
* Compiler / Interpreter: [The provided interpreter](http://tome.gytis.co/)
* Notes: The interpreter seems to run faster in Google Chrome.

### Euler 121
* File: [problem121.emoji](problem121.emoji)
* Language: [Emoji](https://esolangs.org/wiki/Emoji)
* Compiler / Interpreter: [The provided interpreter](https://github.com/vpzomtrrfrt/emoji.py)
* Notes: Screenshot is available at [etc/problem121_screenshot.png](etc/problem121_screenshot.png).

### Euler 122
* File: [problem122.vb](problem122.vb)
* Language: [Visual Basic .NET](https://en.wikipedia.org/wiki/Visual_Basic_.NET)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#vb-core)
* Notes: (None)

### Euler 123
* File: [problem123.wen](problem123.wen)
* Language: [Wenyan](https://github.com/wenyan-lang/wenyan)
* Compiler / Interpreter: [The online IDE](https://ide.wy-lang.org/)
* Notes: The answer is outputted in Classical Chinese.

### Euler 124
* File: [problem124.ics](problem124.ics)
* Language: [IntercalScript](https://github.com/Storyyeller/IntercalScript)
* Compiler / Interpreter: [The provided compiler](https://github.com/Storyyeller/IntercalScript)
* Notes: (None)

### Euler 125
* File: [problem125.icon](problem125.icon)
* Language: [Icon](https://www2.cs.arizona.edu/icon/)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#icon)
* Notes: A somewhat slow solution.

### Euler 126
* File: [problem126.i](problem126.i)
* Language: [i](http://rosettacode.org/wiki/Category:I)
* Compiler / Interpreter: [The provided interpreter](https://github.com/Qlova/ilang)
* Notes: `./etc/build126.sh` may be helpful in building the source; a somewhat slow solution.

### Euler 127
* File: [problem127.nit](problem127.nit)
* Language: [Nit](https://nitlanguage.org/)
* Compiler / Interpreter: [The provided compiler](https://github.com/nitlang/nit)
* Notes: The Nit interpreter is prohibitively slow in running this problem; use the compiler `nitc` instead.

### Euler 128
* File: [problem128.alice](problem128.alice)
* Language: [Alice](https://github.com/m-ender/alice)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#alice)
* Notes: A somewhat slow solution.

### Euler 129
* File: [problem129.befreak](problem129.befreak)
* Language: [Befreak](http://tunes.org/~iepos/befreak.html)
* Compiler / Interpreter: [The provided interpreter](http://tunes.org/~iepos/befreak.tar.gz)
* Notes: Code was written with the help of a [Scala translator program](https://github.com/Mercerenies/befreakgen); a very slow solution.

### Euler 130
* File: [problem130.four](problem130.four)
* Language: [Fourier](https://esolangs.org/wiki/Fourier)
* Compiler / Interpreter: [Archived interpreter](https://archive.codeplex.com/?p=fourierlang)
* Notes: Code was written with the help of a [Haskell translator program](etc/frgen.hs); an incredibly slow solution.

### Euler 131
* File: [problem131.perchance](problem131.perchance)
* Language: [Perchance](https://perchance.org/welcome)
* Compiler / Interpreter: [The online interpreter](https://perchance.org/minimal#edit)
* Notes: (None)

### Euler 132
* File: [problem132.gw](problem132.gw)
* Language: [Gwion](https://gwion.github.io/Gwion/index.html)
* Compiler / Interpreter: [The provided interpreter](https://github.com/Gwion/Gwion)
* Notes: (None)

### Euler 133
* File: [problem133.ahk](problem133.ahk)
* Language: [AutoHotkey](https://www.autohotkey.com/)
* Compiler / Interpreter: [AutoHotkey](https://www.autohotkey.com/)
* Notes: (None)

### Euler 134
* File: [problem134.dfy](problem134.dfy)
* Language: [Dafny](https://www.microsoft.com/en-us/research/project/dafny-a-language-and-program-verifier-for-functional-correctness/)
* Compiler / Interpreter: [Dafny 3.2.0](https://github.com/dafny-lang/dafny/releases)
* Notes: `./etc/build134.sh` may be helpful in building the source.

### Euler 135
* File: [problem135.tovie](problem135.tovie)
* Language: [Tovie](https://github.com/Jaysmito101/tovie)
* Compiler / Interpreter: [Tovie v_03](https://github.com/Jaysmito101/tovie/releases/tag/v_03)
* Notes: A somewhat slow solution; must be run using the Python transpiler (`./tovie -f python ./problem135.tovie`), as the default compiler's `malloc` implementation is broken.

### Euler 136
* File: [problem136.v](problem136.v)
* Language: [Verilog](https://en.wikipedia.org/wiki/Verilog)
* Compiler / Interpreter: [Verilator 4.224](https://www.veripool.org/verilator/)
* Notes: `./etc/build136.sh` may be helpful in building the source.

### Euler 137
* File: [problem137.zip](problem137.zip)
* Language: [Minecraft](https://www.minecraft.net/en-us)
* Compiler / Interpreter: [Minecraft Java Edition 1.18.2](https://minecraft.fandom.com/wiki/Java_Edition_1.18.2)
* Notes: Zip contains a world save file; toggle the start lever to re-run the calculation. Answer is displayed in hexadecimal. A very slow solution. Screenshots are available at [etc/Minecraft](etc/Minecraft).

### Euler 138
* File: [problem138.pickle](problem138.pickle)
* Language: [Pickle](https://docs.python.org/3/library/pickle.html#module-pickle)
* Compiler / Interpreter: [Python 3.7.9](https://www.python.org/)
* Notes: `./etc/run138.py` may be helpful in running the source; code was written with the help of a [Lisp translator program](etc/pkgen.lisp).

### Euler 139
* File: [problem139.vale](problem139.vale)
* Language: [Vale](https://vale.dev/)
* Compiler / Interpreter: [Vale 0.2 Kuma](https://vale.dev/download)
* Notes: (None)

### Euler 140
* File: [problem140.dip](problem140.dip)
* Language: [Dip](http://www.dip-lang.org/)
* Compiler / Interpreter: [Try Dip](http://www.dip-lang.org/try)
* Notes: (None)

### Euler 141
* File: [problem141.wat](problem141.wat)
* Language: [WebAssembly](https://webassembly.org/)
* Compiler / Interpreter: [wasmer 2.3.0](https://wasmer.io/)
* Notes: (None)

### Euler 142
* File: [problem142.ark](problem142.ark)
* Language: [ArkScript](https://github.com/ArkScript-lang/Ark/)
* Compiler / Interpreter: [ArkScript 3.4.0](https://github.com/ArkScript-lang/Ark/#building)
* Notes: (None)

### Euler 143
* File: [problem143.ts](problem143.ts)
* Language: [TypeScript](https://www.typescriptlang.org/)
* Compiler / Interpreter: [tsc version 4.9.5](https://www.typescriptlang.org/download)
* Notes: (None)

### Euler 144
* File: [problem144.japt](problem144.japt)
* Language: [Japt](https://github.com/ETHproductions/japt)
* Compiler / Interpreter: [Japt 1.4.6 interpreter](https://ethproductions.github.io/japt/)
* Notes: (None)

### Euler 145
* File: [problem145.txt](problem145.txt)
* Language: [Game Builder Garage](https://www.nintendo.com/store/products/game-builder-garage-switch/)
* Compiler / Interpreter: [Game Builder Garage Ver 1.0.1](https://www.nintendo.com/store/products/game-builder-garage-switch/)
* Notes: Nintendo Switch required to run; screenshots are available at [etc/GameBuilderGarage](etc/GameBuilderGarage).

### Euler 146
* File: [problem146.x10](problem146.x10)
* Language: [X10](http://x10-lang.org/)
* Compiler / Interpreter: [X10 Release 2.6.2](https://sourceforge.net/projects/x10/)
* Notes: A somewhat slow solution.

### Euler 147
* File: [problem147.lean](problem147.lean)
* Language: [Lean Theorem Prover](https://leanprover-community.github.io/)
* Compiler / Interpreter: [Lean (version 3.42.1, commit 68455b087d87, Release)](https://leanprover-community.github.io/get_started.html)
* Notes: `./etc/build147.sh` may be helpful in building the source.

### Euler 148
* File: [problem148.trn](problem148.trn)
* Language: [TRANSCRIPT](https://esolangs.org/wiki/TRANSCRIPT)
* Compiler / Interpreter: [The official interpreter](https://web.archive.org/web/20071018030927/http://www.corknut.org/code/transcript/)
* Notes: (None)

### Euler 149
* File: [problem149.z](problem149.z)
* Language: [Z](https://zlanguage.github.io/)
* Compiler / Interpreter: [@zlanguage/zcomp/0.4.5 linux-x64 node-v14.18.2](https://www.npmjs.com/package/@zlanguage/zcomp)
* Notes: (None)

### Euler 150
* File: [problem150.moon](problem150.moon)
* Language: [MoonScript](https://moonscript.org/)
* Compiler / Interpreter: [MoonScript version 0.5.0](rocks-5.3/moonscript/dev-1/bin/)
* Notes: (None)

### Euler 151
* File: [problem151.dry](problem151.dry)
* Language: [Dry](https://github.com/melvic-ybanez/dry)
* Compiler / Interpreter: [The official interpreter (02e9cbf)](https://github.com/melvic-ybanez/dry/tree/02e9cbfebcb88c9429a55468d5d1d6acb8250acf)
* Notes: Provided solution does not round the result off to six digits.

### Euler 152
* File: [problem152.erl](problem152.erl)
* Language: [Erlang](https://www.erlang.org/)
* Compiler / Interpreter: [Erlang/OTP 25](https://www.erlang.org/downloads)
* Notes: (None)

### Euler 153
* File: [problem153.m](problem153.m)
* Language: [GNU Octave](https://octave.org/)
* Compiler / Interpreter: [GNU Octave, version 7.3.0](https://octave.org/download)
* Notes: A very slow solution.

### Euler 154
* File: [problem154.eg](problem154.eg)
* Language: [Earl Grey](https://github.com/breuleux/earl-grey)
* Compiler / Interpreter: [Earl Grey version 0.1.2](https://github.com/breuleux/earl-grey)
* Notes: An incredibly slow solution.

### Euler 155
* File: [problem155.cs](problem155.cs)
* Language: [C#](https://dotnet.microsoft.com/en-us/languages/csharp)
* Compiler / Interpreter: [Mono JIT compiler version 6.12.0.182](https://www.mono-project.com/download/stable/)
* Notes: (None)

### Euler 156
* File: [problem156.arc](problem156.arc)
* Language: [Arc](https://en.wikipedia.org/wiki/Arc_(programming_language))
* Compiler / Interpreter: [Arcadia 0.25.1](https://github.com/kimtg/Arcadia)
* Notes: (None)

### Euler 157
* File: [problem157.blsq](problem157.blsq)
* Language: [Burlesque](https://esolangs.org/wiki/Burlesque)
* Compiler / Interpreter: [Burlesque 1753bdf](https://github.com/FMNSSun/Burlesque)
* Notes: A somewhat slow solution; code was written with the help of a [Racket translator program](etc/blsq_gen.rkt).

### Euler 158
* File: [problem158.cf0x10](problem158.cf0x10)
* Language: [Comefrom0x10](https://esolangs.org/wiki/Comefrom0x10)
* Compiler / Interpreter: [The official interpreter](https://comefrom0x10.readthedocs.io/en/latest/install.html)
* Notes: (None)

### Euler 159
* File: [problem159.png](problem159.png)
* Language: [Pikt](https://github.com/iamgio/pikt)
* Compiler / Interpreter: [Pikt 64025d6](https://github.com/iamgio/pikt/tree/64025d690daffe0315654fae74b08af39319d542)
* Notes: `./etc/build159.sh` may be helpful in building the source.

### Euler 160
* File: [problem160.r](problem160.r)
* Language: [Rebol](http://www.rebol.com/)
* Compiler / Interpreter: [REBOL/Core 2.7.8.4.10 (23-Jan-2016)](http://www.rebol.com/downloads.html)
* Notes: (None)

### Euler 161
* File: [problem161.swift](problem161.swift)
* Language: [Swift](https://www.swift.org/)
* Compiler / Interpreter: [swift-5.8.1-RELEASE](https://www.swift.org/download/)
* Notes: (None)

### Euler 162
* File: [problem162.xlsx](problem162.xlsx)
* Language: [Microsoft Excel](https://www.microsoft.com/en-us/microsoft-365/excel)
* Compiler / Interpreter: [Office 365 Excel 16.0.16704.42304](https://www.office.com/)
* Notes: Due to numerical precision limits, the answer is printed along two cells

### Euler 163
* File: [problem163.ts](problem163.ts)
* Language: [AssemblyScript](https://www.assemblyscript.org/)
* Compiler / Interpreter: [assemblyscript 0.19.23](https://www.npmjs.com/package/assemblyscript/v/0.19.23)
* Notes: `./etc/build163.sh` may be helpful in building the source.

### Euler 164
* File: [problem164.dhall](problem164.dhall)
* Language: [Dhall](https://dhall-lang.org/)
* Compiler / Interpreter: [Dhall 1.41.2](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.41.2)
* Notes: (None)

### Euler 165
* File: [problem165.hy](problem165.hy)
* Language: [Hy](http://hylang.org/)
* Compiler / Interpreter: [hy 0.27.0+96.g902233d8](https://github.com/hylang/hy/tree/902233d854d765058927402d60f1ee8bef02daf8)
* Notes: An incredibly slow solution.

### Euler 166
* File: [problem166.rock](problem166.rock)
* Language: [Rockstar](https://github.com/RockstarLang/rockstar)
* Compiler / Interpreter: [wolfgang42/rockstar-js 29a1476](https://github.com/wolfgang42/rockstar-js/tree/29a14768db98724b1ba4f61b27362d0f68558603)
* Notes: Indented version is available at [etc/problem166_indented.rock](etc/problem166_indented.rock).

### Euler 167
* File: [problem167.zig](problem167.zip)
* Language: [Zig](https://ziglang.org/documentation/master/)
* Compiler / Interpreter: [0.12.0-dev.1200+5f92b070b](https://ziglang.org/download/)
* Notes: A very slow solution, recommended to be compiled with `-OReleaseFast`.

### Euler 168
* File: [problem168.11l](problem168.11l)
* Language: [11l](http://11l-lang.org/)
* Compiler / Interpreter: [11l 2022.11](https://github.com/11L-lang/11l)
* Notes: (None)

### Euler 169
* File: [problem169.gspl](problem169.gspl)
* Language: [AGSPL](https://esolangs.org/wiki/AGSPL)
* Compiler / Interpreter: [AGSPL (commit eb6afa05)](https://github.com/xi816/AGSPL/commit/eb6afa05db2c972330b623ccddbd1c81ef7a6759)
* Notes: (None)

### Euler 170
* File: [problem170.pizza](problem170.pizza)
* Language: [Pizza](https://pizzacompiler.sourceforge.net/index.html)
* Compiler / Interpreter: [Pizza v1.0g, 1-October-2001](https://sourceforge.net/projects/pizzacompiler/files/)
* Notes: Pizza compiler requires an old Java runtime to run; tested on Java 1.4.2_19

### Euler 171
* File: [problem171.sh](problem171.sh)
* Language: [Fish](https://fishshell.com/)
* Compiler / Interpreter: [fish, version 3.6.1](https://fishshell.com/)
* Notes: A very slow solution.

### Euler 172
* File: [problem172.bsx](problem172.bsx)
* Language: [Bussin](https://github.com/face-hh/bussin)
* Compiler / Interpreter: [bussin c97bece](https://github.com/face-hh/bussin/commit/c97bece4769c438b8c4bf0f012d9eb33720dd3f4)
* Notes: The interpreter hangs after execution.

### Euler 173
* File: [problem173.folders](problem173.folders)
* Language: [Folders](https://esolangs.org/wiki/Folders)
* Compiler / Interpreter: [Folders.py 0bec545](https://github.com/SinaKhalili/Folders.py/tree/main)
* Notes: Code was written with the help of a [Haskell translator program](etc/generate_173.hs); output of Linux `tree` command is available at `etc/problem173_tree.txt`.

### Euler 174
* File: [problem174.html](problem174.html)
* Language: [Hyperscript](https://hyperscript.org/)
* Compiler / Interpreter: [HyperScript 0.9.12](https://unpkg.com/hyperscript.org@0.9.12) on Firefox 120.0.1
* Notes: (None)

### Euler 175
* File: [problem175.emote](problem175.emote)
* Language: [Emotinomicon](https://github.com/ConorOBrien-Foxx/Emotinomicon/)
* Compiler / Interpreter: [The online interpreter](http://conorobrien-foxx.github.io/Emotinomicon/int.html)
* Notes: A somewhat slow solution; code was written with the help of a [Lisp translator program](etc/emotegen.lisp); screenshot is available at [etc/problem175_screenshot.png](etc/problem175_screenshot.png).

### Euler 176
* File: [problem176.naz](problem176.naz)
* Language: [naz](https://esolangs.org/wiki/Naz)
* Compiler / Interpreter: [naz 82960e5](https://github.com/sporeball/naz)
* Notes: Code was written with the help of a [Racket translator program](etc/naz_gen.rkt); requires the `-u` flag.

### Euler 177
* File: [problem177.coco](problem177.coco)
* Language: [Coconut](http://coconut-lang.org/)
* Compiler / Interpreter: [Coconut: Version 3.0.4](https://coconut.readthedocs.io/en/latest/DOCS.html#installation)
* Notes: A somewhat slow solution.

### Euler 178
* File: [problem178.k](problem178.k)
* Language: [K](https://en.wikipedia.org/wiki/K_(programming_language))
* Compiler / Interpreter: [ngn/k on TIO Nexus](https://tio.run/#k-ngn)
* Notes: (None)

### Euler 179
* File: [problem179.whirl](problem179.whirl)
* Language: [Whirl](https://web.archive.org/web/20130116204525/bigzaphod.org/whirl/)
* Compiler / Interpreter: [Whirl-2.java](https://web.archive.org/web/20120303124619/http://www.bigzaphod.org/whirl/Whirl-2.java)
* Notes: An incredibly slow solution, code was written with the help of a [Racket translator program](https://github.com/Mercerenies/whirlgen).

### Euler 180
* File: [problem180.py](problem180.py)
* Language: [Pyf\*\*ck](https://github.com/wanqizhu/pyfuck)
* Compiler / Interpreter: [Python 3.11.6](https://www.python.org/downloads/)
* Notes: Code was generated with the help of the creator's [translation tool](https://github.com/wanqizhu/pyfuck/blob/master/pyfuck.py).

### Euler 181
* File: [problem181.oa](problem181.oa)
* Language: [Oasis](https://github.com/oasis-lang/oasis)
* Compiler / Interpreter: [oasis @ 9c3bed49](https://github.com/oasis-lang/oasis/tree/9c3bed49471267272c9558b8f8b96b6d87262630)
* Notes: (None)

### Euler 182
* File: [problem182.br](problem182.br)
* Language: [Broccoli](https://github.com/mathieucaroff/broccoli)
* Compiler / Interpreter: [https://mathieucaroff.com/broccoli](https://mathieucaroff.com/broccoli)
* Notes: (None)

### Euler 183
* File: [problem183.pyra](problem183.pyra)
* Language: [Pyramid Scheme](https://github.com/ConorOBrien-Foxx/Pyramid-Scheme)
* Compiler / Interpreter: [Pyramid-Scheme @ fd183d29](https://github.com/ConorOBrien-Foxx/Pyramid-Scheme/tree/fd183d296f08e0cba8bf55da907697eaf412f6a7)
* Notes: Code was written with the help of a [Scala translator program](https://github.com/Mercerenies/pyramid-scheme-generator).

### Euler 184
* File: [problem184.L42](problem184.L42)
* Language: [42](https://forty2.is/index.xhtml)
* Compiler / Interpreter: [L42PortableLinux.zip](https://forty2.is/download.xhtml)
* Notes: `./etc/build184.sh` may be helpful in building the source.

### Euler 185
* File: [problem185.lp](problem185.lp)
* Language: [Potassco](https://potassco.org/)
* Compiler / Interpreter: [clingo](https://potassco.org/clingo/)
* Notes: The answer outputs `correct(N, X)`, where `X` is the `N`th digit in the result.

### Euler 186
* File: [problem186.odin](problem186.odin)
* Language: [Odin](https://odin-lang.org/)
* Compiler / Interpreter: [Odin dev-2024-07](https://github.com/odin-lang/Odin/releases/tag/dev-2024-07)
* Notes: Compiler may require [a patch](https://github.com/odin-lang/Odin/issues/2271#issuecomment-1959009449) on some Linux systems.

### Euler 187
* File: [problem187.ms](problem187.ms)
* Language: [Minus](http://www.golfscript.com/minus/index.html)
* Compiler / Interpreter: [iminus.c](http://www.golfscript.com/minus/files/iminus.c)
* Notes: A very slow solution, will need to increase the `memsize` constant to compile (`#define mem_size 300000000` is sufficient), code was written with the help of a [Racket translator program](etc/minusgen.rkt).

### Euler 188
* File: [problem188.jgrid](problem188.jgrid)
* Language: [Javagrid](https://esolangs.org/wiki/Javagrid)
* Compiler / Interpreter: [The online interpreter](https://stefan-hering.github.io/)
* Notes: An incredibly slow solution, [this Python script](https://gist.github.com/Mercerenies/7da15bd8663d8ccde11c5b5bb7d22bb3) can be useful for automating insertion into the web UI.

### Euler 189
* File: [problem189.ox](problem189.ox)
* Language: [Oxide](https://github.com/tuqqu/oxide-lang/tree/master)
* Compiler / Interpreter: [oxide-lang at 5088a2d](https://github.com/tuqqu/oxide-lang/tree/5088a2d7278608b67114263c42f13e7206e2e338)
* Notes: A very slow solution.

### Euler 190
* File: [problem190.act](problem190.act)
* Language: [Actually](https://github.com/Mego/Seriously/)
* Compiler / Interpreter: [TIO Nexus](https://tio.run/#actually)
* Notes: (None)

### Euler 191
* File: [problem191.emot](problem191.emot)
* Language: [Emoticon](https://esolangs.org/wiki/Emoticon)
* Compiler / Interpreter: [teuton.org](https://www.teuton.org/~stranger/code/emoticon/interpreter.php)
* Notes: (None)

### Euler 192
* File: [problem192.du](problem192.du)
* Language: [Dictu](https://dictu-lang.com/)
* Compiler / Interpreter: [dictu-lang @ e43580d2](https://github.com/dictu-lang/Dictu/tree/e43580d2e1660d0215651dc94da98ae75d372a98)
* Notes: (None)

## Valid Languages

Mostly, I am just using common sense to determine what constitutes a "language". In particular,
a language does not have to be Turing-complete in order for me to consider using it for this challenge.
Here are just a few of the guidelines I am following. These are in no way permanent or binding and will
change as I progress.
* BF-derivatives and BF-clones are considered identical to Brainf\*\*k and will not be used.
* Lisp derivatives are considered distinct. That is, there will be a Racket, a Common Lisp, and an
  ELisp, which are all distinct languages.
* Sufficiently different BASIC dialects will be considered distinct. That is, Visual BASIC, Liberty
  BASIC, and QBASIC are distinct languages, but Liberty BASIC and JustBASIC are one and the same.
* The result should be in a normal, human readable form. For example, in BF it is insufficient to
  print the ASCII character associated with the integer result; it is required to print the result as
  a number. There is no specific required format, as some languages may include some garbage around
  the answer, so long as the answer as outputted is clear and readable. If the language is incapable
  of outputting a human readable value, whatever is considered acceptable output within the language
  will be acceptable.
* I have written a few programming languages. At least one of them is in a usable state right now.
  For fairness, I will not be using any language that I myself wrote.
* Generally speaking, interpreters and compilers are to be taken
  "as-is". That means that, while it is perfectly permissible to use
  compiler-specific extensions to a language, it is also necessary to
  treat any bugs in the chosen compiler as intended behavior. One
  minor exception to this is in the case of historical code, where
  minor modifications are necessary to get a language interpreter
  written long ago to work on a modern computer, as there are several
  historical languages I aim to use for this challenge.

## Speed Adjectives

Some solutions are marked as "slow". Generally speaking, the following
rough guidelines can be used to estimate how long it takes to run the
code on a reasonably modern computer. These are, of course, very rough
estimates, so take them with a grain of salt.

* 30s or less: I will generally not denote these at all.
* 30s to 2min: "A somewhat slow solution"
* 2min to 10min: "A very slow solution"
* More than 10min: "An incredibly slow solution"

Currently, the slowest solutions in this repository are:
* Problem 179 (Whirl) in ~34 minutes
* Problem 130 (Fourier) in ~26 minutes
* Problem 96 (Javagony) in ~20 minutes
* Problem 188 (Javagrid) in ~18 minutes

Currently, the slowest solution in this repository is Problem 130
(Fourier), which takes roughly 26 minutes. The second slowest is
Problem 96 (Javagony), at roughly 20 minutes.

I do not have an explicit upper bound on the runtime of a correct
algorithm. I will not leave code unattended overnight or for days on
end, and the code written must *actually* terminate on a machine I
have access to, not just theoretically produce correct results.

## Final Notes

You'll notice that I'm not using many of the mainstream languages so far. This is not because I'm
trying to make this challenge harder than it is; this is a purely strategic decision. The later
Project Euler problems are more difficult, so I am saving the powerful and easy-to-use languages
for them and using obscure, esoteric languages for the easier problems.
