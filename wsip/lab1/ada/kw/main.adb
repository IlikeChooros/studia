with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Main is
    
    a: Float := 1.0;
    b: Float := 2.0;
    c: Float := -3.0;

    procedure solve_quadratic(a: Float; b: Float; c: Float) is
        delta_q: Float;
        x1: Float;
        x2: Float;
    begin
        
        if a = 0.0 then
            Put_Line("a = 0.0, nie jest funckją kwadratową");
            return;
        end if;

        delta_q := b**2 - 4.0*a*c;

        -- Bez rozwiązań (w rzeczywistych)
        if delta_q < 0.0 then
            Put_Line("Bez rozwiązań");

        -- Jedno rozwiązanie, x1 = x2
        elsif delta_q = 0.0 then
            x1 := -b / (2.0*a);
            Put("Jedno rozwiązanie: x1=x2=");
            Put(x1, Fore => 1, Aft => 4, Exp => 0);

        -- Dwa rozwiązania
        else
            delta_q := Sqrt(delta_q);
            x1 := (-b + delta_q) / (2.0*a);
            x2 := (-b - delta_q) / (2.0*a);
            -- Formatowanie z dokładnością do 4 miejsc po przecinku
            Put("Dwa rozwiązania: x1=");
            Put(x1, Fore => 1, Aft => 4, Exp => 0);
            Put(", x2=");
            Put(x2, Fore => 1, Aft => 4, Exp => 0);
        end if;

    end solve_quadratic;
    
begin

    Put_Line("Podaj współczynniki ax^2 + bx + c = 0");

    Put("a: ");
    Get(a);

    Put("b: ");
    Get(b);

    Put("c: ");
    Get(c);

    solve_quadratic(a, b, c);

end Main;