with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Primes;


procedure Main is
   
   N           : Integer;
   Result_Int  : Integer;
   Result_Bool : Boolean;

begin

   if Argument_Count /= 2 then
      Put_Line("Niepoprawna liczba argumentów, użycie: ./primes_main ip|pr|pn <liczba>");
      return;
   end if;
   
   -- Deklaracja zmiennych, bo nie można deklarować Stringów wcześniej
   -- (przynajmniej nie wiem jak)
   declare
      Command : String := Argument(1);
      N_Str   : String := Argument(2);
   begin

      -- Sprawdzenie czy argument jest liczbą, coś jak
      -- try-catch w cpp
      begin
         N := Integer'Value(Argument(2));
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

      if Command = "ip" then
         Result_Bool := Primes.Is_Prime(N);
         if Result_Bool then
            Put_Line("true");
         else
            Put_Line("false");
         end if;

      elsif Command = "pr" then
         Result_Int := Primes.Nth_Prime(N);
         Put_Line(Result_Int'Image);
      
      elsif Command = "pn" then
         Result_Int := Primes.Primes_Up_To_N(N);
         Put_Line(Result_Int'Image);
      
      else
         Put_Line("Niepoprawna komenda: " & Command & ", użycie: ./primes_main ip|pr|pn <liczba>");
      end if;

   end;

end Main;