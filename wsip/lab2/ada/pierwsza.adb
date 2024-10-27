with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Main is
    
    n: Integer;

    function is_prime(n: Integer) return Boolean is
        root: Integer := Integer(Sqrt(Float(n)));
        i: Integer := 5;
    begin

        -- Jeżeli n jest niepoprawne zwróć `False`
        if n <= 0 then
            return False;
        end if;

        -- Szczególne przypadki
        if n = 2 or n = 3 then
            return True;
        end if;

        -- Sparwdzanie podzielności przez 3 i 2 (6)
        if n mod 2 = 0 or n mod 3 = 0 then
            return False;
        end if;

        -- Sprawdzanie podzielności przez pozostałe liczby
        loop
            exit when i > root;

            if n mod i = 0 or n mod (i + 3) = 0 or n mod(i + 2) = 0 then
                return False;
            end if;

            i := i + 6;
        end loop;
        
        -- Jest liczbą pierwszą
        return True;
    end is_prime;

begin
    
    Put("Podaj liczbę: ");
    Get(n);

    if is_prime(n) then
        Put_Line("Liczba" & n'Image & " jest pierwsza");
    else 
        Put_Line(n'Image & " nie jest pierwsza");
    end if;

end Main;