with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;
with Ada.Containers.Doubly_Linked_Lists;

package Utils is
   type Tri_State is (False, True, Undecided);
   Assumption_Error : exception;

   function To_Tri_State (Truth : String) return Tri_State;

   package String_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Unbounded_String);

   procedure Put_List (List : String_Lists.List);

   package Hash_Strings is
      new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 10);


   subtype String_List is String_Lists.List;
   subtype Hash_String_Type is Hash_Strings.Bounded_String;

   function To_Hash_String (S : String) return Hash_String_Type;
end Utils;
