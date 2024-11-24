with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Main is
    
    function binomial_coefficient(n: Integer; k: Integer) return Long_Long_Integer is
        coeffs    : array(0 .. k) of Long_Long_Integer;
        temp      : Long_Long_Integer;
        prev_copy : Long_Long_Integer;
        min       : Integer;
    begin
        
        --  Wadliwe dane
        if n < k then
            return 0;
        end if;

        --  Wartości początkowe
        coeffs(0) := 1;
        for i in 1 .. k loop
            coeffs(i) := 0;
        end loop;

        --  Obliczanie współczynników
        for i in 1 .. n loop
            --  Zapisanie wartości poprzedniej
            prev_copy := coeffs(0);

            --  Wybór mniejszej wartości (i lub k),
            --  aby nie wykonywać niepotrzebnych operacji
            min := i;
            if i > k then
                min := k;
            end if;

            -- Można by robic do i, ale wtedy trzeba by było
            -- poszerzyć tablicę o dodatkowe miejsce do n,
            -- oraz obliczlibyśmy wartości, które nie są nam potrzebne (i > k)
            for j in 1 .. min loop
                temp := coeffs(j);
                coeffs(j) := prev_copy + coeffs(j);
                prev_copy := temp;
            end loop;

            -- Koniec trojkąta Pascala
            if i <= k then
                coeffs(i) := 1;
            end if;

        end loop;

        return coeffs(k);
    end binomial_coefficient;


    n      : Integer;
    k      : Integer;
    result : Long_Long_Integer;

begin
    
    Put("Podaj n: ");
    Get(n);
    Put("Podaj k: ");
    Get(k);

    result := binomial_coefficient(n, k);
    Put_Line("Symbol newtona (n, k):" & result'Image);

end Main;