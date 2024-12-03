with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Totient;

procedure Main is
    N: Integer;
    Result_Int: Integer;

begin

   if Argument_Count < 1 then
      Put_Line("Niepoprawna liczba argumentów, użycie: ./main <liczba1> <liczba2> ...");
      return;
   end if;

   for I in 1 .. Argument_Count loop
      declare
         N_Str: String := Argument(I);
      begin

         -- Sprawdzenie czy argument jest liczbą, coś jak
         -- try-catch w cpp
         begin
            N := Integer'Value(Argument(I));
         exception
            when others =>
               Put_Line("Niepoprawny argument, podaj liczbę całkowitą");
               return;
         end;

         -- Sprawdzenie czy liczba jest większa od 0
         if N <= 0 then
            Put_Line("Liczba musi być większa od 0");
            return;
         end if;

         Result_Int := Totient.Totient(N);
         Put_Line("totient(" & N_Str & ") =" & Result_Int'Image);

      end;
   end loop;

end Main;