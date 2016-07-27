	IDENTIFICATION DIVISION.
	PROGRAM-ID.	 Problem32.

	DATA DIVISION.
	WORKING-STORAGE SECTION.
	01	Accum PIC 99999.
	01	Counter PIC 99999.
	01	Upper PIC 99999.
	01	Candidate PIC 99999.
	01	Candidate0 PIC 99999.
	01	Str0 PIC X(6).
	01	Str1 PIC X(6).
	01	Str2 PIC X(6).
	01	StrC PIC X(20).
	01	Temp PIC 9.
	01	TempS PIC X(1).
	01	Tally PIC 99.
	01	Okay PIC 9.

	PROCEDURE DIVISION.
		COMPUTE Accum = 0.
		PERFORM VARYING Counter FROM 1 BY 1 UNTIL 9999 < Counter
			COMPUTE Upper = Counter / 2
			PERFORM VARYING Candidate FROM 1 BY 1
				UNTIL Candidate > Upper
				IF FUNCTION Mod(Counter, Candidate) = 0
					MOVE Counter TO Candidate0
					DIVIDE Candidate INTO Candidate0
					MOVE Candidate TO Str0
					MOVE Candidate0 TO Str1
					MOVE Counter TO Str2
					STRING
						Str0 DELIMITED BY SIZE
						Str1 DELIMITED BY SIZE
						Str2 DELIMITED BY SIZE
						INTO StrC
					MOVE 1 TO Okay
					PERFORM VARYING Temp FROM 1 BY 1 UNTIL 8 < Temp
						MOVE Temp TO TempS
						MOVE 0 TO Tally
						INSPECT StrC TALLYING Tally FOR ALL TempS
						IF Tally NOT = 1
						   MOVE 0 TO Okay
						END-IF
					END-PERFORM
					MOVE Temp TO TempS
					MOVE 0 TO Tally
					INSPECT StrC TALLYING Tally FOR ALL TempS
					IF Tally NOT = 1
					   MOVE 0 TO Okay
					END-IF
					IF Okay = 1
						ADD Counter TO Accum
						EXIT PERFORM
					END-IF
				END-IF
			END-PERFORM
		END-PERFORM.
		DISPLAY Accum.
		STOP RUN.
