

package body list is 

   
   -- Create a new node
   function Create_Node(X: Integer) return NodePtr is
      New_Node: NodePtr;
   begin
      New_Node := new Node;
      New_Node.Elem := X;
      return New_Node;
   end Create_Node;

   -- Find a node by index, 1 <= I <= Lenght, may return null
   function Find_Node(L: ListT; I: Integer) return NodePtr is
      Node: NodePtr;
      J: Integer := I;
   begin
      
      J := J - 1;
      if J >= L.Lenght then
         return null;
      end if;

      Node := L.First;
      while J > 0 loop
         Node := Node.Next;
         J := J - 1;
      end loop;

      return Node;
   end Find_Node;

   procedure Insert(L: in out ListT; I: Integer; X: Integer) is
      Node    : NodePtr := L.First;
      Prev    : NodePtr := L.First;
      New_Node: NodePtr;
      J       : Integer := I;
   begin

      J := J - 1;
      if J > L.Lenght then
         return;
      end if;

      New_Node := Create_Node(X);

      while (J > 0 and Node /= null) loop
         Prev := Node;
         Node := Node.Next;
         J    := J - 1;
      end loop;

      -- Insert at the beginning
      if Node = L.First then
         New_Node.Next := L.First;
         L.First       := New_Node;

      -- Insert at the end
      elsif Node = null then
         Prev.Next     := New_Node;
         New_Node.Next := Node;

      -- Middle insert
      else
         Prev.Next     := New_Node;
         New_Node.Next := Node;
      end if;

      -- Increase the length
      L.Lenght := L.Lenght + 1;

   end Insert;

   -- Add an element to the end of the list
   procedure Append(L: in out ListT; X: Integer) is
   begin
      Insert(L, L.Lenght + 1, X);
   end Append;

   -- Push an element to the top of the list
   procedure Push(L: in out ListT; X: Integer) is
   begin
      Insert(L, 1, X);
   end Push;


   -- Get an element by index, 1 <= I <= Lenght
   function Get(L: ListT; I: Integer) return Integer is
      Node: NodePtr;
   begin
      Node := Find_Node(L, I);
      if Node = null then
         return 0;
      end if;

      return Node.Elem;
   end Get;

   -- Put an element by index, 1 <= I <= Lenght
   procedure Put(L: in out ListT; I: Integer; X: Integer) is
      Node: NodePtr;
   begin
      Node := Find_Node(L, I);
      if Node = null then
         return;
      end if;

      Node.Elem := X;
   end Put;

   -- Delete an element by index, 1 <= I <= Lenght
   procedure Delete(L: in out ListT; I: Integer) is
      Node: NodePtr := L.First;
      Prev: NodePtr := L.First;
      J   : Integer := I;
   begin

      J := J - 1;
      if J > L.Lenght then
         return;
      end if;

      while (J > 0 and Node /= null) loop
         Prev := Node;
         Node := Node.Next;
         J    := J - 1;
      end loop;

      -- Delete the first element
      if Node = L.First then
         L.First := Node.Next;

      -- Delete the last element
      elsif Node = L.Last then
         Prev.Next := null;
         L.Last    := Prev;

      -- Delete a middle element
      else
         Prev.Next := Node.Next;
      end if;

      -- Decrease the length
      L.Lenght := L.Lenght - 1;

      -- Free the memory
      Free(Node);

   end Delete;

   -- Pop an element from the top of the list
   function Pop(L: in out ListT) return Integer is
      Result: Integer;
   begin
      Result := Get(L, 1);
      Delete(L, 1);
      return Result;
   end Pop;

   -- Check if the list is empty
   function Is_Empty(L: ListT) return Boolean is
   begin
      return L.Lenght = 0;
   end Is_Empty;

   -- Print the list
   procedure Print(L: ListT) is
      Node: NodePtr := L.First;
   begin
      while Node /= null loop
         Put(Integer'Image(Node.Elem) & " ");
         Node := Node.Next;
      end loop;

      Put_Line("(size:" & Integer'Image(L.Lenght) & ")");
   end Print;

   -- Clean the list
   procedure Clean(L: in out ListT) is
      Node: NodePtr := L.First;
      Next: NodePtr;
   begin
      while Node /= null loop
         Next := Node.Next;
         Free(Node);
         Node := Next;
      end loop;
      L.First  := null;
      L.Last   := null;
      L.Lenght := 0;
   end Clean;

   -- Get the length of the list (by counting the elements)
   function Length(L: ListT) return Integer is
      Node: NodePtr := L.First;
      Count: Integer := 0;
   begin
      
      while Node /= null loop
         Count := Count + 1;
         Node  := Node.Next;
      end loop;

      return Count;
   end Length;

end list;