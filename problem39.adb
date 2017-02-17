with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure problem39 is
   Max_Per : Integer := 0;
   Max_Val : Integer := 0;
   Solns : Integer := 0;
   C : Integer := 0;
begin
   for P in 1 .. 1000 loop
      Solns := 0;
      for A in 1 .. P loop
         for B in A .. (P - A) loop
            C := P - A - B;
            if A ** 2 + B ** 2 = C ** 2 then
               Solns := Solns + 1;
            end if;
         end loop;
      end loop;
      if Solns > Max_Val then
         Max_Val := Solns;
         Max_Per := P;
      end if;
   end loop;
   Put (Max_Per);
   Put_Line ("");
end problem39;
