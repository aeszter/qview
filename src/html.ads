with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package HTML is
   procedure Put_Cell (Data : String; Tag : String := "td");
   --  write out a single table cell, optionally changing the tag (to th)
   procedure Put_UCell (Data : Unbounded_String);
   --  write out a single table cell
   procedure Put_UCell_With_Link (Data : Unbounded_String; Link_Param : String);
   --  write out a single table cell, linking the contents

   procedure Put_Navigation_Begin;
   procedure Put_Navigation_End;
   procedure Put_Navigation_Link (Data : String; Link_Param : String);
   --  navigation section

   procedure Put_Paragraph (Label : String; Contents : String);
   procedure Put_Paragraph (Label : String; Contents : Unbounded_String);
   procedure Put_Paragraph (Label : Unbounded_String; Contents : Unbounded_String);
   procedure Put_True_False (Truth : String);
   procedure Put_True_False (Truth : Unbounded_String);

end HTML;
