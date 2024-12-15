with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure main is
   A: Long_Float := 0.0;
   B: Long_Float := 0.0;
   Eps: Long_Float := 0.0;

   function F(X: Long_Float) return Long_Float is
   begin
      return Cos(X / 2.0);
   end F;

   type FuncType is access function(X: Long_Float) return Long_Float;

   -- Find a zero of the function F in the interval [A, B] with the given precision Eps
   -- Using the bisection method
   function Find_Zero(F: FuncType; A: in out Long_Float; B: in out Long_Float; Eps: Long_Float) return Long_Float is
      X0: Long_Float := 0.0;
   begin
      if F(A) * F(B) > 0.0 then
         raise Constraint_Error with "No zero in the interval";
      end if;

      -- Swap A and B if B < A
      if B < A then
         X0 := A;
         A := B;
         B := X0;
      end if;

      while (B - A) > Eps loop
         X0 := (A + B) / 2.0;

         if F(A) * F(X0) < 0.0 then
            B := X0;
         else
            A := X0;
         end if;
      end loop;

      return X0;
   end Find_Zero;

begin

   Put("Enter the interval [A, B]: ");
   Get(A);
   Get(B);

   Put("Enter the precision Eps: ");
   Get(Eps);

   Put("The zero of the function is: ");
   Put(Find_Zero(F'Access, A, B, Eps), Width => 0, Fore => 1);

end main;