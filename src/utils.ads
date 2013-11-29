with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with POSIX; use POSIX;

package Utils is
   Version : String := "v1.7"; -- Update Bugzilla when you change this
   type Tri_State is (False, True, Undecided);
   Assumption_Error : exception;

   type Fixed is delta 0.0001 digits 5;
   --  a general fixed type, especially useful for SGE resources

   type Usage_Number is delta 0.0001 digits 18;
   type Usage_Integer is range 0 .. 10 ** 12;


   function To_Tri_State (Truth : String) return Tri_State;

   package String_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Unbounded_String);

   package String_Sets is
     new Ada.Containers.Ordered_Sets (Element_Type => Unbounded_String);

   package String_Pairs is
     new Ada.Containers.Ordered_Maps (Key_Type => Unbounded_String,
                                      Element_Type => Unbounded_String);

   package Hash_Strings is
     new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 10);

   package ID_Lists is
     new Ada.Containers.Ordered_Sets (Element_Type => Natural,
                                      "<"          => "<",
                                      "="          => "=");


   subtype String_List is String_Lists.List;
   subtype ID_List is ID_Lists.Set;
   subtype Hash_String_Type is Hash_Strings.Bounded_String;

   function To_Hash_String (S : String) return Hash_String_Type;

   procedure To_String_List (Source  : String; Dest : out POSIX_String_List);

end Utils;
