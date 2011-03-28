with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;

package Utils is
   type Tri_State is (False, True, Undecided);
   Assumption_Error : exception;

   function To_Tri_State (Truth : String) return Tri_State;

   package String_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Unbounded_String);


   subtype String_List is String_Lists.List;
end Utils;
