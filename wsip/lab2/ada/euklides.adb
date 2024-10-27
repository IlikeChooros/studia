with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

    a, b: Integer;
    
    -- @param a liczba naturalna
    -- @param b liczba naturalna
    -- @return NWD(a, b)
    function euklides(
        a: Integer;
        b: Integer
    ) return Integer is
        temp: Integer;
        x: Integer := a;
        y: Integer := b;
    begin

        -- Sprawdzamy czy dane podane są poprawne
        if x < 0 or y < 0 then
            return 0;
        end if;

        loop  
            -- Warunek wyjścia
            exit when y <= 0;

            -- zamiana a i b
            if x < y then
                temp := x;
                x := y;
                y := temp;
            end if;

            x := x - y;
        end loop;

        return x;

    end euklides;

begin

    Put_Line("Podaj 2 liczby: ");
    Get(a);
    Get(b);

    Put_Line("NWD = " & euklides(a, b)'Image);
    
end Main;