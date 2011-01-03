with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Calendar;

package HTML is
   procedure Put_Cell (Data       : String;
                       Link_Param : String := "";
                       Tag        : String := "td";
                       Class      : String := "");
   --  write out a single table cell, optionally changing the tag (to th)
   --  optionally, link the contents as "Link_Param=Data"
   procedure Put_Cell (Data       : Unbounded_String;
                       Link_Param : String := "";
                       Tag        : String := "td";
                       Class      : String := "");
   procedure Put_Img_Cell (Image : String);
   procedure Put_Time_Cell (Time : Calendar.Time);
   procedure Put_Header_Cell (Data     : String;
                              Params   : Unbounded_String;
                              Sortable : Boolean := True);


   procedure Put_Navigation_Begin;
   procedure Put_Navigation_End;
   procedure Put_Navigation_Link (Data : String; Link_Param : String);
   --  navigation section

   procedure Put_Paragraph (Label : String; Contents : String);
   procedure Put_Paragraph (Label : String; Contents : Unbounded_String);
   procedure Put_Paragraph (Label : Unbounded_String; Contents : Unbounded_String);
   procedure Put_True_False (Truth : String);
   procedure Put_True_False (Truth : Unbounded_String);

   procedure Put_Stylesheet (URL : String);
   procedure Put_Clearer;

   function Param_Is (Param : String; Expected : String) return Boolean;

end HTML;
