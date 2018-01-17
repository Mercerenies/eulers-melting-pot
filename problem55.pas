Program Problem55;

Uses sysutils, math;

Type pstring = ^string;

Function IsPalindrome(s : string) : boolean;
Var
   i   : integer;
   len : integer;
Begin
   len := Length(s);
   For i := 1 To len div 2 + 1 Do
      Begin
         If s[i] <> s[len - i + 1] Then
            Begin
               IsPalindrome := false;
               Exit;
            End
      End;
   IsPalindrome := true;
End;

Procedure Reverse(s : string; s1 : pstring);
Var
   i : integer;
Begin
   s1^ := '';
   For i := 1 To Length(s) Do
      s1^ := s[i] + s1^;
End;

{ Bare-bones bignum implementation, since Pascal's integers
  are not big enough. }
Procedure AddValues(s1, s2 : string; s3 : pstring);
Var
   i     : integer;
   carry : integer;
   acurr : integer;
   bcurr : integer;
   curr  : integer;
Begin
   carry := 0;
   For i := 1 To Max(Length(s1), Length(s2)) Do
   Begin
      If i > Length(s1) Then
         acurr := 0
      Else
         acurr := Ord(s1[Length(s1) - i + 1]) - Ord('0');
      If i > Length(s2) Then
         bcurr := 0
      Else
         bcurr := Ord(s2[Length(s2) - i + 1]) - Ord('0');
      curr := acurr + bcurr + carry;
      carry := curr div 10;
      s3^ := IntToStr(curr mod 10) + s3^;
   End;
   If carry <> 0 Then
      s3^ := IntToStr(carry) + s3^;
End;

Procedure NextValue(s : string; out : pstring);
Var
   s1 : string;
Begin
   s1 := '';
   Reverse(s, @s1);
   AddValues(s, s1, out);
End;

Function IsLychrel(x : integer) : boolean;
Var
   s  : string ;
   s1 : string ;
   i  : integer;
Begin
   s := IntToStr(x);
   For i := 1 To 50 Do
      Begin
         s1 := '';
         NextValue(s, @s1);
         s := s1;
         If IsPalindrome(s) Then
            Begin
               IsLychrel := false;
               Exit;
            End
      End;
   IsLychrel := true;
End;

Var
   count : integer;
   i     : integer;
   temp  : string ;
Begin
   count := 0;
   For i := 1 To 9999 Do
      Begin
         If IsLychrel(i) Then
            count := count + 1;
      End;
   WriteLn(count);
End.
