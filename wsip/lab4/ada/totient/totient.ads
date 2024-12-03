
package Totient is

   function Totient (N : Integer) return Integer;

private

   -- Wymagane typy dla tablicy
    type dist_array is array(Positive range <>) of Integer;
    type dist_array_access is access dist_array; -- To jest pointer na `dist_array`

    -- Struktura z wynikiem
    type Prime_Factors_Result is record
        Distribution: dist_array_access;
        Size: Integer;
    end record;

   function Prime_Factors (N: Integer) return Prime_Factors_Result;
end Totient;