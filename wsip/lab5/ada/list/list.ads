with Ada.Unchecked_Deallocation;

package list is 

   type ListT is private;

   function Is_Empty(L: ListT) return Boolean;

   function Pop(L: in out ListT) return Integer;
   procedure Push(L: in out ListT; X: Integer);
   procedure Append(L: in out ListT; X: Integer);

   function Get(L: ListT; I: Integer) return Integer;
   procedure Put(L: in out ListT; I: Integer; X: Integer);
   procedure Insert(L: in out ListT; I: Integer; X: Integer);
   procedure Delete(L: in out ListT; I: Integer);

   procedure Print(L: ListT);
   procedure Clean(L: in out ListT);
   function Length(L: ListT) return Integer;

private

   type Node;
   type NodePtr is access Node;

   function Create_Node(X: Integer) return NodePtr;
   function Find_Node(L: ListT; I: Integer) return NodePtr;

   type Node is record
      Elem: Integer := 0;
      Next: NodePtr := null;
   end record;

   type ListT is record
      First : NodePtr := null;
      Last  : NodePtr := null;
      Length: Integer := 0;
   end record;

   procedure Free is 
      new Standard.Ada.Unchecked_Deallocation(Node, NodePtr);

end list;