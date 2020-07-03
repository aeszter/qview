with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Ranges; use SGE.Ranges;

--  @summary
--  Represent SGE Slot Ranges.
--
--  @description
--  This package provides routines that deal with SGE Slot Ranges.
--  Actual data manipulation is handled by SGElib, so this package
--  handles the user interface (i.e. printing).
--
package Ranges is

   procedure Put (S : Step_Range; Label : String);
--  Output one slot range as a paragraph.
--  @param S The slot range to print
--  @param Label A name or description given to S

   procedure Put_Cell (Data : Step_Range_List; Class : String);
--  Output a list of slot ranges as a table cell.
--  @param Data The list to print
--  @param Class The CSS class to attach to the cell
end Ranges;
