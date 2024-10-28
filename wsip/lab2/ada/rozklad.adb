with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

    -- Wymagane typy dla tablicy
    type dist_array is array(Positive range <>) of Integer;
    type dist_array_access is access dist_array; -- To jest pointer na `dist_array`

    -- Struktura z wynikiem
    type Prime_Distribution_Result is record
        Distribution: dist_array_access;
        Size: Integer;
    end record;

    -- Funkcja zwracająca tablicę z rozkładem liczby na czynniki pierwsze
    function prime_distribution(n: Integer) return Prime_Distribution_Result is
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
    end prime_distribution;


    -- Wypisywanie czynników pierwszych w formie n^pow
    procedure Print_Format(n: Integer; pow: Integer; first: Boolean) is
    begin

        if not first then
            Put("*");
        end if;

        -- Jeśli potęga jest większa od 1 to wypisz potęgę
        if pow > 1 then
            -- Nie używam Put(n'Image & "^" & pow'Image) bo to nie działa
            -- z jakiegoś powodu deweloperzy Ady pomyśleli, że dobrym pomysłem
            -- jest dodawanie spacji na początku liczby...
            Put(n, Width => 0); -- Width => 0, bo inaczej Ada dodaje spacje (w chuj)
            Put("^");
            Put(pow, Width => 0);
        else
            Put(n, Width => 0);
        end if;

    end Print_Format;

    -- Wypisuje rozkład liczby na czynniki pierwsze
    -- w formie n^pow * n^pow * ..., np. 25 = 5^2, 100 = 2^2 * 5^2, 13 = 13
    procedure Print_Distribution(Result: Prime_Distribution_Result) is
        occurances: Integer := 0;
        first: Boolean := True;
        prev: Integer;
        i: Integer := 1;
    begin

        -- Ada liczy od 1 (bo jest upośledzona), więc size = 1 oznacza brak
        -- elementów w tablicy
        if Result.Size = 1 then
            Put_Line("Brak czynników pierwszych");
            return;
        end if;

        prev := Result.Distribution(1);

        while i < Result.Size loop
            if prev = Result.Distribution(i) then
                occurances := occurances + 1;
                i := i + 1;
            else 
                Print_Format(prev, occurances, first);
                first := False;
                occurances := 0;
                prev := Result.Distribution(i);
            end if;
        end loop;

        -- Zawsze zostanie jeszcze jeden element
        Print_Format(prev, occurances, first);
        New_Line; -- coś jak printf("\n");

    end Print_Distribution;


    n: Integer;
    Result: Prime_Distribution_Result;

begin
    Put("Podaj liczbę: ");
    Get(n);
    Result := prime_distribution(n);
    Print_Distribution(Result);

end Main;