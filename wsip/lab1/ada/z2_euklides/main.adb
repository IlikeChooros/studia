with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
    a : Integer;
    b : Integer;
    c : Integer;
    nwd: Integer;
begin

    Put("Podaj pierwsza liczbe: ");
    Get(a);
    Put("Podaj druga liczbe: ");
    Get(b);

    loop 
        if b = 0 then
            nwd := a;
            exit;
        end if;

        c := a mod b;
        a := b;
        b := c;
    end loop;

    Put_Line("NWD: " & nwd'Image);
end Main;