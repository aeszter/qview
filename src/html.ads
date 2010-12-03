with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package HTML is
   procedure Put_Cell (Data : String; Tag : String := "td");
   procedure Put_UCell (Data : Unbounded_String);
   procedure Put_UCell_With_Link (Data : Unbounded_String; Link_Param : String);
   procedure Put_Navigation_Begin;
   procedure Put_Navigation_End;
   procedure Put_Navigation_Link (Data : String; Link_Param : String);
end HTML;
