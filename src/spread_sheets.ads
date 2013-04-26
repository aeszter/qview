with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Spread_Sheets is
   type Cell is
      record
         Contents : Unbounded_String := Null_Unbounded_String;
         Line_Separator : Boolean := False;
      end record;
   package Cell_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Cell);
   subtype Cell_List is Cell_Lists.List;
private

end Spread_Sheets;
