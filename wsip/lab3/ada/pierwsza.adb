with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Main is
    
    n: Integer;

    type dist_array is array(Integer range <>) of Boolean;
    type dist_array_access is access dist_array; -- To jest pointer na `dist_array`

    -- Zlicza liczby pierwsze w tablicy sita
    function count_primes(arr: dist_array_access) return Integer is
        count: Integer := 0;
    begin

        for i in arr'range loop
            if arr(i) = True then
                count := count + 1;
            end if;
        end loop;
    
        return count;
    end count_primes;


    function primes_up_to_n(n: Integer) return Integer is
        root: Integer;
        j: Integer := 0;
        sieve: dist_array_access := null;
    begin

        -- Jeżeli n jest niepoprawne zwróć `False`
        if n <= 0 then
            return -1;
        end if;

        root  := Integer(Sqrt(Float(n))) + 1;
        sieve := new dist_array(0 .. n + 1);
        sieve(0) := False;
        sieve(1) := False;

        for i in 2 .. n loop
            sieve(i) := True;
        end loop;

        for i in 2 .. root loop
            if sieve(i) = True then
                j := i * i;

                while j <= n loop
                    sieve(j) := False;
                    j        := j + i;
                end loop;
            end if;
        end loop;
        
        -- Jest liczbą pierwszą
        return count_primes(sieve);
    end primes_up_to_n;

begin
    
    Put("Podaj liczbę: ");
    Get(n);

    declare
        result: Integer := primes_up_to_n(n);
    begin
        if result = -1 then
            Put_Line("Liczba " & n'Image & " jest niepoprawna");
        else
            Put_Line("Jest " & result'Image & " liczb pierwszych mniejszych lub równych " & n'Image);
        end if;
    end;

end Main;