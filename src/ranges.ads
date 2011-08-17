with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Ranges is

   type Step_Range is record
      Min, Step, Max : Natural;
   end record;

   function New_Range (Min, Step, Max : Natural)
                          return Step_Range;
   ---------
   -- Put --
   --  Purpose: Output one slot range as a paragraph
   --  Parameter S: The slot range to print
   ---------
   procedure Put (S : Step_Range);

   package Range_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Step_Range);

   --------------
   -- Put_Cell --
   --  Purpose: Output a list of slot ranges as a table cell
   --  Parameter Data: The list to print
   --  Parameter Class: The CSS class to attach to the cell
   ---------
   procedure Put_Cell (Data : Range_Lists.List; Class : String);

   ---------------
   -- To_String --
   --  Purpose: Format the contents of a slot range (Slots) as a String
   --  Parameter What: The slot range to format
   --  Parameter Short: Whether to collapse ranges "n - n" to "n"
   --  Returns: The formatted String
   ---------------

   function To_String (What : Step_Range; Short : Boolean) return String;

   ---------------
   -- To_Unbounded_String --
   --  Purpose: Format the contents of a slot range (Slots) as a String
   --  Parameter What: The slot range to format
   --  Parameter Short: Whether to collapse ranges "n - n" to "n"
   --  Returns: The formatted String
   ---------------
   function To_Unbounded_String (What : Step_Range; Short : Boolean) return Unbounded_String;

   function Hash (List : Range_Lists.List) return String;
   function Hash (S : Step_Range) return Hash_Type;

end Ranges;
