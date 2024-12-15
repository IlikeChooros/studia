with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with list;

procedure main is
   l : list.ListT;
   r : Integer;
   index : Integer;
   command : Unbounded_String;
   continue : Boolean := True;

   -- Funkcja pobierająca indeks elementu na liście, zwraca True jeśli indeks jest poprawny
   -- Is_Insert - czy funkcja jest wywoływana w celu wstawienia elementu (wtedy indeks może być równy długości listy)
   -- Index - zmienna do której zapisywany jest indeks
   function Get_Index (Is_Insert: Boolean; Index: in out Integer) return Boolean is
      Int_Is_Insert : Integer := 0;
      begin
         Put ("Index: ");
         Get(Index);
         Skip_Line;

         if Is_Insert then
            Int_Is_Insert := 1;
         end if;

         if Index > 0 and Index <= (list.Length(l) + Int_Is_Insert) then
            return True;
         end if;

         Put_Line ("Error - index out of range!");
         return False;
   end Get_Index;

   -- Funkcja pobierająca wartość z konsoli
   procedure Get_Value (result: in out Integer) is
      begin
         Put ("Value: ");
         Get(result);
         Skip_Line;
   end Get_Value;

   -- Print result OK
   procedure Result_OK is
   begin
      Put_Line ("Result: OK");
   end Result_OK;

begin
   while continue loop
      Put ("Command: ");
      Get_Line (command);

      -- Komendy usuwania
      if command = "Pop" then
         if not list.Is_Empty (l) then
            r := list.Pop (l);
            Put_Line ("Result: " & r'Image);
         else
            Put_Line ("Error - stack is empty!");
         end if;

      elsif command = "Delete" then
         if Get_Index(False, index) then
            list.Delete (l, index);
            Result_OK;
         end if;

      elsif command = "Clean" then
         list.Clean (l);
         Result_OK;
      
      -- Komendy dodawania
      elsif command = "Push" then
         Get_Value(r);
         list.Push (l, r);
         Result_OK;

      elsif command = "Append" then
         Get_Value(r);
         list.Append (l, r);
         Result_OK;

      elsif command = "Insert" then
         if Get_Index(True, index) then
            Get_Value(r);
            list.Insert (l, index, r);
            Result_OK;
         end if;

      -- Komendy modyfikacji
      elsif command = "Put" then
         if Get_Index(False, index) then
            Get_Value(r);
            list.Put (l, index, r);
            Result_OK;
         end if;

      -- Komendy zwracające wartości
      elsif command = "Get" then
         if Get_Index(False, index) then
            r := list.Get (l, index);
            Put_Line ("Result: " & r'Image);
         end if;

      elsif command = "Length" then
         r := list.Length (l);
         Put_Line ("Result: " & r'Image);

      -- Wyświetlanie listy
      elsif command = "Print" then
         Put ("Result: ");
         list.Print (l);
      
      -- Wyjście
      elsif command = "Exit" or command = "q" or command = "quit" then
         continue := False;
      else
         Put_Line ("Unknown command!");
      end if;
   end loop;

   --  clean list
   while not list.Is_Empty (l) loop
      r := list.Pop (l);
   end loop;
end main;
