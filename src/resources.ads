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
   procedure Put (R : Resource);



   package Resource_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Resource);
end Resources;
