with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;


procedure Main is
    
    function nth_prime(n: Integer) return Integer is
        count, num, root: Integer;
        is_prime: Boolean;
        primes: array(1 .. n) of Integer;
    begin

        -- Niepoprawne dane wejściowe
        if n < 1 then
            return 0;
        end if;

        -- Inicjalizacja tablicy liczb pierwszych
        primes(1) := 2;
        count := 1;
        num := 3;

        while count < n loop
            is_prime := True;
            root := Integer(Sqrt(Float(num)));

            for i in 1 .. count loop

                -- Jeśli liczba jest większa od pierwiastka z liczby,
                -- to nie ma sensu sprawdzać dalej
                if primes(i) > root then
                    exit;
                end if;

                -- Jeśli liczba jest podzielna przez jakąś liczbę pierwszą,
                -- to nie jest liczbą pierwszą
                if num mod primes(i) = 0 then
                    is_prime := False;
                    exit;
                end if;

            end loop;

            -- Dodaj liczbę do tablicy liczb pierwszych
            if is_prime then
                primes(count) := num;
                count := count + 1;
            end if;

            num := num + 2;

        end loop;

        return primes(count - 1);
    end nth_prime;


    n: Integer;
    result: Integer;

begin
    
    Put("Podaj n: ");
    Get(n);

    result := nth_prime(n);

    Put(n, Width => 0);
    Put_Line("-ta liczba pierwsza: " & result'Image);

end Main;