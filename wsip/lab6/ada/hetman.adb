with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Hetman is

   type Array_B is array (Integer range <>) of Boolean;
   type Ptr_Array_B is access Array_B;

   type Array_T is array (Integer range <>) of Integer;
   type Ptr_Array_T is access Array_T;

   -- Print the array
   procedure Print_Array (Arr : Ptr_Array_T) is
   begin
      for I in Arr'Range loop
         Put (Arr (I)'Image & " ");
      end loop;
      New_Line;
   end Print_Array;

   -- Check if the permutation is a valid solution to n-queens problem
   function Is_Valid (Perm : Ptr_Array_T) return Boolean is
   begin
      for I in Perm'Range loop
         for J in I + 1 .. Perm'Last loop
            if abs (Perm (I) - Perm (J)) = J - I then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Valid;

   -- Recursive function to generate permutations for valid n-queens solutions
   function Permutation
     (N : Integer; K : Integer; Perm : Ptr_Array_T; Used : Ptr_Array_B)
      return Integer
   is
      N_Solutions : Integer := 0;
   begin
      if K = N then
         if Is_Valid (Perm) then
            Print_Array (Perm);
            return 1;
         end if;
      else
         for I in Used'Range loop
            if not Used (I) then
               Perm (K) := I + 1;
               Used (I) := True;
               N_Solutions := N_Solutions + Permutation (N, K + 1, Perm, Used);
               Used (I) := False;
            end if;
         end loop;
      end if;

      return N_Solutions;
   end Permutation;

   -- Main function to generate n-queens solutions
   function Queen_Perm (N : Integer) return Integer is
      Perm : Ptr_Array_T := new Array_T (0 .. N);
      Used : Ptr_Array_B := new Array_B (0 .. N);
   begin
      for I in Perm'Range loop
         Perm (I) := 1;
      end loop;

      for I in Used'Range loop
         Used (I) := False;
      end loop;

      return Permutation (N, 0, Perm, Used);
   end Queen_Perm;


   -- Put
   function Put_Queen
     (Sq        : Integer;
      Position  : Ptr_Array_T;
      Att_Row   : Ptr_Array_B;
      Att_LDiag : Ptr_Array_B;
      Att_RDiag : Ptr_Array_B) return Integer
   is
      N_Solutions : Integer := 0;
   begin
      for I in Position'Range loop
         if not Att_Row (I)
           and not Att_LDiag (I - Sq)
           and not Att_RDiag (I + Sq)
         then

            Position (Sq) := I;
            Att_Row (I) := True;
            Att_LDiag (I - Sq) := True;
            Att_RDiag (I + Sq) := True;

            if Sq = Position'Last then
               Print_Array (Position);
               return 1;
            else
               N_Solutions :=
                 N_Solutions
                 + Put_Queen (Sq + 1, Position, Att_Row, Att_LDiag, Att_RDiag);
            end if;

            Att_Row (I) := False;
            Att_LDiag (I - Sq) := False;
            Att_RDiag (I + Sq) := False;
         end if;
      end loop;
      return N_Solutions;
   end Put_Queen;

   -- Main function to generate n-queens solutions
   function N_Queen (N : Integer) return Integer is
      Position  : Ptr_Array_T := new Array_T (1 .. N);
      Att_Row   : Ptr_Array_B := new Array_B (1 .. N);
      Att_LDiag : Ptr_Array_B := new Array_B (-N + 1 .. N - 1);
      Att_RDiag : Ptr_Array_B := new Array_B (2 .. 2 * N);
   begin

      for I in Att_LDiag'Range loop
         Att_LDiag (I) := False;
      end loop;

      for I in Att_RDiag'Range loop
         Att_RDiag (I) := False;
      end loop;

      for I in Att_Row'Range loop
         Att_Row (I) := False;
      end loop;

      for I in Position'Range loop
         Position (I) := 0;
      end loop;

      return Put_Queen (1, Position, Att_Row, Att_LDiag, Att_RDiag);
   end N_Queen;

   N           : Integer := 8;
   N_Solutions : Integer;

begin

   if Argument_Count /= 2
     or (Argument (1) /= "perm" and Argument (1) /= "back")
   then
      Put_Line ("Usage: hetman [back | perm] <n>");
      return;
   end if;

   N := Integer'Value (Argument (2));
   if Argument (1) = "back" then
      N_Solutions := N_Queen (N);
   else
      N_Solutions := Queen_Perm (N);
   end if;

   Put_Line ("Number of solutions:" & N_Solutions'Image);

end Hetman;
