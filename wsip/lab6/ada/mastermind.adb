with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Mastermind is
   
   type Array_T is array (Integer range <>) of Integer;
   
   -- Print the array
   procedure Print_Guess (Numbers : Array_T; N_Guess: Integer) is
      Guess : Integer := Numbers(Numbers'First);
      Digit : Integer;
   begin
      Put(N_Guess'Image & ":");
      for I in 1 .. 4 loop
         Digit := (Guess / (10 ** (4 - I))) mod 10;
         Put(Digit'Image);
      end loop;
      Put_Line(" ?");
   end Print_Guess;


   -- Generate all possible permutations of 4 digits
   procedure Generate_Permutations (Numbers : in out Array_T) is
   begin
      for I in Numbers'Range loop
         for K in 1 .. 4 loop
            -- Convert I to a 4-digit number in base 6
            Numbers(I) := Numbers(I) + ((I / (6 ** (4 - K))) mod 6) * 10 ** (4 - K);
         end loop;
         Put_Line (I'Image & ": " & Numbers(I)'Image);
      end loop;
   end Generate_Permutations;

   -- Solve one step of the game
   procedure Solve 
      (Feedback_In: Integer; Feedback_Out: Integer; Numbers: in out Array_T; N_Size: in out Integer) is
      N_Filtered : Integer := 0;
      Guess : Integer := Numbers(0);

      Code : Integer; 
      In_Place : Integer;
      Out_Of_Place : Integer;
      Digit_Guess : Integer;
      Digit_Code : Integer;
      Digit_Guess_K : Integer;
   begin
      
      for I in 0 .. N_size - 1 loop
         Code := Numbers (I);
         In_Place := 0;
         Out_Of_Place := 0;

         for J in 1 .. 4 loop
            Digit_Code  := ( Code / (10 ** (4 - J))) mod 10;
            Digit_Guess := (Guess / (10 ** (4 - J))) mod 10;

            -- Digits are the same as in the guess, so 
            if Digit_Code = Digit_Guess then
               In_Place := In_Place + 1;
            else 
            -- Count out of place digits
               for K in 1 .. 4 loop
                  Digit_Guess_K := ( Guess / (10 ** (4 - K))) mod 10;

                  if Digit_Guess_K = Digit_Code then
                     Out_Of_Place := Out_Of_Place + 1;
                     exit;
                  end if;
               end loop; 
            end if;
         end loop;

         -- Matching guess
         if In_Place = Feedback_In and Out_Of_Place = Feedback_Out then
            Numbers (N_Filtered) := Numbers (I);
            N_Filtered := N_Filtered + 1;
         end if;
      end loop;

      N_Size := N_Filtered;
   end Solve;


   Numbers : Array_T(0 .. 1295) := (others => 1111); -- 1296 = 6^4
   N_Size  : Integer            := 1296;

   N_Guess : Integer            := 1;
   Feedback_In  : Integer       := 0;
   Feedback_Out : Integer       := 0;
begin

   Generate_Permutations (Numbers => Numbers);

   while not (Feedback_In = 4 or N_Size = 0) loop
      Print_Guess(Numbers, N_Guess);
      
      Put ("Na swoim miejscu: ");
      Get (Feedback_In);
      Put ("Nie na swoim miejscu: ");
      Get (Feedback_Out);

      Solve(Feedback_In, Feedback_Out, Numbers, N_Size);
      N_Guess := N_Guess + 1;
   end loop;

   if N_Size = 1 then
      Put_Line ("Wygrałem.");
   else
      Put_Line ("Oszukujesz.");
   end if;

end Mastermind;