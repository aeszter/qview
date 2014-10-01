with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Ranges; use SGE.Ranges;


package Ranges is

   ---------
   -- Put --
   --  Purpose: Output one slot range as a paragraph
   --  Parameter S: The slot range to print
   ---------
   procedure Put (S : Step_Range; Label : String);

   --------------
   -- Put_Cell --
   --  Purpose: Output a list of slot ranges as a table cell
   --  Parameter Data: The list to print
   --  Parameter Class: The CSS class to attach to the cell
   ---------
   procedure Put_Cell (Data : Step_Range_List; Class : String);
end Ranges;
