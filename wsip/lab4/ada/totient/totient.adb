with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Totient is 

   function Totient (N : Integer) return Integer is
      Factors: Prime_Factors_Result;
      Result : Integer := 1;
   begin

      Factors := Prime_Factors (N);

      for I in 1 .. Factors.Size - 1 loop
         
         if I = 1 then
            Result := Result * (Factors.Distribution(I) - 1);
         elsif Factors.Distribution(I) /= Factors.Distribution(I - 1) then
            Result := Result * (Factors.Distribution(I) - 1);
         else
            Result := Result * Factors.Distribution(I);
         end if;

      end loop;

      return Result;

   end Totient;


    -- Funkcja zwracająca tablicę z rozkładem liczby na czynniki pierwsze
    function Prime_Factors(n: Integer) return Prime_Factors_Result is
        num: Integer := n;
        div: Integer;
        root: Integer;
        size: Integer := 1;
        log2_n: Integer;
        distribution: dist_array_access := null;
    begin

        -- Nieprawidłowe dane
        if n < 2 then
            return (distribution, 1);
        end if;

        -- Jeżeli n >= 2, to dopiero wtedy alokuj pamięć
        log2_n := Integer(Log(Float(n), Base => 2.0));
        -- log2(n) to maksymalna ilość czynników pierwszych
        distribution := new dist_array(1 .. log2_n); 
        div := 2;

        -- Podzielność przez 2
        while num mod div = 0 loop
            num := num / div;
            distribution(size) := div;
            size := size + 1;
        end loop;

        div := 3;
        root := Integer(Sqrt(Float(num)));

        -- Liczenie podzielności przez liczby nieparzyste
        while div <= root loop

            -- Dopóki n jest podzielne przez div
            while num mod div = 0 loop
                num := num / div;
                distribution(size) := div;
                size := size + 1;
                root := Integer(Sqrt(Float(num)));
            end loop;

            -- Przechodzenie do kolejnej liczby nieparzystej
            div := div + 2;
        end loop;

        -- Jeśli n jest liczbą pierwszą
        if num > 1 then
            distribution(size) := num;
            size := size + 1;
        end if;

        return (distribution, size);
    end Prime_Factors;

end Totient;
