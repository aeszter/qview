with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Resources is

   type Resource is record
      Name : Unbounded_String;
      Value : Unbounded_String;
   end record;

   type Network is (none, eth, ib);

   function New_Resource (Name : Unbounded_String; Value : Unbounded_String)
     return Resource;
   function New_Resource (Name : String; Value : String)
     return Resource;
   procedure Put (R : Resource);



   package Resource_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Resource);

   procedure Sort (L : in out Resource_Lists.List);
   function Equal (Left, Right : Resource_Lists.List) return Boolean;
   function Precedes (Left, Right : Resource) return Boolean;
   function "<" (Left, Right : Resource) return Boolean;
   function Precedes (Left, Right : Resource_Lists.List) return Boolean;

   function To_Unbounded_String (L : Resource_Lists.List) return Unbounded_String;

   package Sorting is
      new Resource_Lists.Generic_Sorting ("<" => Precedes);

end Resources;
