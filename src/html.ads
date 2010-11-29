with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package HTML is
   procedure Put_Cell (Data : String; Tag : String := "td");
   procedure Put_UCell (Data : Unbounded_String);
end HTML;
