with Ada.Text_IO; use Ada.Text_IO;

package body HTML is

   --------------
   -- Put_Cell --
   --------------

   procedure Put_Cell (Data : String; Tag : String := "td") is
   begin
      Put_Line ("<" & Tag & ">" & Data & "</" & Tag & ">");
   end Put_Cell;

   procedure Put_UCell (Data : Unbounded_String) is
   begin
      Put_Cell (To_String (Data));
   end Put_UCell;

end HTML;
