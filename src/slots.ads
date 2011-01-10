with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Slots is

   type Slots is record
      Min, Step, Max : Natural;
   end record;

   function New_Range (Min, Step, Max : Natural)
                          return Slots;
   procedure Put (S : Slots);



   package Slot_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Slots);
end Slots;
