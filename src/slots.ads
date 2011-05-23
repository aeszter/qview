with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Slots is

   type Slots is record
      Min, Step, Max : Natural;
   end record;

   function New_Range (Min, Step, Max : Natural)
                          return Slots;
   ---------
   -- Put --
   --  Purpose: Output one slot range as a paragraph
   --  Parameter S: The slot range to print
   ---------
   procedure Put (S : Slots);

   package Slot_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Slots);

   --------------
   -- Put_Cell --
   --  Purpose: Output a list of slot ranges as a table cell
   --  Parameter Data: The list to print
   --  Parameter Class: The CSS class to attach to the cell
   ---------
   procedure Put_Cell (Data : Slot_Lists.List; Class : String);

   ---------------
   -- To_String --
   --  Purpose: Format the contents of a slot range (Slots) as a String
   --  Parameter What: The slot range to format
   --  Parameter Short: Whether to collapse ranges "n - n" to "n"
   --  Returns: The formatted String
   ---------------

   function To_String (What : Slots; Short : Boolean) return String;

   ---------------
   -- To_Unbounded_String --
   --  Purpose: Format the contents of a slot range (Slots) as a String
   --  Parameter What: The slot range to format
   --  Parameter Short: Whether to collapse ranges "n - n" to "n"
   --  Returns: The formatted String
   ---------------
   function To_Unbounded_String (What : Slots; Short : Boolean) return Unbounded_String;

   function Hash (List : Slot_Lists.List) return String;
   function Hash (S : Slots) return Hash_Type;

end Slots;
