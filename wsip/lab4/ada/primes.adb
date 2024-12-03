with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Primes is

   -- Funkcja sprawdzająca czy liczba jest pierwsza
   function Is_Prime (N : Integer) return Boolean is
      Root: Integer;
      I   : Integer := 2;
   begin

      if N <= 1 then
         return False;
      end if;

      if N = 2 or N = 3 then
         return True;
      end if;

      if N mod 2 = 0 or N mod 3 = 0 then
         return False;
      end if;

      Root := Integer (Sqrt (Float (N)));

      while I <= Root loop
         if (N mod I = 0) or (N mod (I + 2) = 0) or (N mod (I + 3) = 0) then
            return False;
         end if;
         I := I + 6;
      end loop;

      return True;

   end Is_Prime;

   -- Funkcja zwracająca liczbę liczb pierwszych mniejszych od N
   function Primes_Up_To_N (N : Integer) return Integer is
      Sieve: array (2 .. N) of Boolean := (others => True);
      Count: Integer := 0;
      J : Integer;
   begin

      if N < 2 then
         return 0;
      end if;

      -- Sito Eratostenesa
      for I in 2 .. Integer(Sqrt(Float(N))) loop
         if Sieve(I) then
            J := I * I;
            while J <= N loop
               Sieve(J) := False;
               J := J + I;
            end loop;
         end if;
      end loop;

      -- Zliczanie liczb pierwszych, 0,1 pomijamy
      for I in Sieve'Range loop
         if Sieve(I) then
            Count := Count + 1;
         end if;
      end loop;

      return Count;
   end Primes_Up_To_N;

   -- Funkcja zwracająca n-tą liczbę pierwszą
   function Nth_Prime (N : Integer) return Integer is
        Count, Num, Root: Integer;
        Is_prime: Boolean;
        Primes: array(1 .. N) of Integer;
    begin

      -- Niepoprawne dane wejściowe
      if N < 1 then
         return 0;
      end if;

      -- Inicjalizacja tablicy liczb pierwszych
      Primes(1) := 2;
      Count := 1;
      Num := 3;

      while Count < N loop
         Is_prime := True;
         Root := Integer(Sqrt(Float(Num)));

         for i in 1 .. Count loop

               -- Jeśli liczba jest większa od pierwiastka z liczby,
               -- to nie ma sensu sprawdzać dalej
               if Primes(i) > Root then
                  exit;
               end if;

               -- Jeśli liczba jest podzielna przez jakąś liczbę pierwszą,
               -- to nie jest liczbą pierwszą
               if Num mod Primes(i) = 0 then
                  Is_prime := False;
                  exit;
               end if;

         end loop;

         -- Dodaj liczbę do tablicy liczb pierwszych
         if Is_prime then
               Primes(count) := Num;
               count := count + 1;
         end if;

         Num := Num + 2;

      end loop;

      return Primes(count - 1);
   end Nth_Prime;

end Primes;