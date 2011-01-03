with Ada.Text_IO; use Ada.Text_IO;
with CGI;
with Ada.Calendar;
with GNAT.Calendar;
with GNAT.Calendar.Time_IO; use GNAT.Calendar.Time_IO;

package body HTML is

   --------------
   -- Put_Cell --
   --  Purpose: Write a table cell with given contents.
   --  Input: Data: Contents of the cell;
   --  Input: Tag: (optional) tag to use instead of <td> (e.g. <th>)
   --  Input: Link_Param: (optional) CGI Parameter to use in link, sc.
   --        <a href="the_url?Link_Param=Data">Data</a>
   --------------

   procedure Put_Cell (Data       : String;
                       Link_Param : String := "";
                       Tag        : String := "td";
                       Class      : String := "") is
      Open_Tag : Unbounded_String;
      --  This is not ideal: we are using an unbounded string without any real need,
      --  just in order to avoid four separate cases of default parameters
      Close_Tag : String := "</" & Tag & ">";
   begin
      if Class /= "" then
         Open_Tag := To_Unbounded_String ("<" & Tag & " class=""" & Class & """>");
      else
         Open_Tag := To_Unbounded_String ("<" & Tag & ">");
      end if;

      if Link_Param = "" then
         Put_Line (To_String (Open_Tag) & Data & Close_Tag);
      else
         Put_Line (To_String (Open_Tag) & "<a href=""" & CGI.My_URL & "?"
                   & Link_Param & "=" & Data & """>"
                   & Data & "</a>" & Close_Tag);
      end if;
   end Put_Cell;

   procedure Put_Cell (Data       : Unbounded_String;
                       Link_Param : String := "";
                       Tag        : String := "td";
                       Class      : String := "") is
   begin
      Put_Cell (Data       => To_String (Data),
                Link_Param => Link_Param,
                Tag        => Tag,
                Class      => Class);
   end Put_Cell;

   procedure Put_Img_Cell (Image : String) is
      Data : String := "<img src=""/icons/" & Image
           & ".png"" alt=""" & Image & """ title="""
           & Image & """ />";
   begin
      Put_Cell (Data => Data);
   end Put_Img_Cell;

   procedure Put_Time_Cell (Time : Calendar.Time) is
      Year, This_Year   : Ada.Calendar.Year_Number;
      Month, This_Month : Ada.Calendar.Month_Number;
      Day, This_Day     : Ada.Calendar.Day_Number;
      Secs              : Ada.Calendar.Day_Duration;
   begin
      Ada.Calendar.Split (Date    => Time,
                          Year    => Year,
                          Month   => Month,
                          Day     => Day,
                          Seconds => Secs);
      Ada.Calendar.Split (Date    => Ada.Calendar.Clock,
                          Year    => This_Year,
                          Month   => This_Month,
                          Day     => This_Day,
                          Seconds => Secs);
      if Day = This_Day and then Month = This_Month and then Year = This_Year then
         Put_Cell (Data => "today, " & Image (Time, "%H:%M:%S"));
      else
         Put_Cell (Data => Image (Time, ISO_Date & " %H:%M:%S"));
      end if;
   end Put_Time_Cell;


   procedure Put_Header_Cell (Data     : String;
                              Params   : Unbounded_String;
                              Sortable : Boolean := True) is
   begin
      if not Sortable then
         Put_Cell (Data => Data,
                   Tag  => "th");
      elsif Params = "" then
         Put_Cell (Data       => Data,
                   Link_Param => "sort",
                   Tag        => "th",
                   Class      => "sorter"
                  );
      elsif Param_Is ("sort", Data) then -- Sorted by this queue, Params exist

         Put_Cell (Data       => Data,
                   Link_Param => To_String (Params & "&sort"),
                   Tag        => "th",
                   Class      => "sorter_active"
                  );
      else
         Put_Cell (Data       => Data,
                   Link_Param => To_String (Params & "&sort"),
                   Tag        => "th",
                   Class      => "sorter"
                  );
      end if;
   end Put_Header_Cell;


   procedure Put_Navigation_Begin is
   begin
      Ada.Text_IO.Put_Line ("<div id=""navigation""><ul>");
   end Put_Navigation_Begin;

   procedure Put_Navigation_End is
   begin
      Ada.Text_IO.Put_Line ("</ul></div id=""navigation"">");
   end Put_Navigation_End;

   procedure Put_Navigation_Link (Data : String; Link_Param : String) is
   begin
      Ada.Text_IO.Put_Line ("<li><a href=""" & CGI.My_URL
                            & "?" & Link_Param & """>" & Data & "</a>");
   end Put_Navigation_Link;

   procedure Put_Paragraph (Label : String; Contents : String) is
   begin
      Ada.Text_IO.Put_Line ("<p>" & Label & ": " & Contents & "</p>");
   end Put_Paragraph;

   procedure Put_Paragraph (Label : String; Contents : Unbounded_String) is
   begin
      Put_Paragraph (Label, To_String (Contents));
   end Put_Paragraph;

   procedure Put_Paragraph (Label : Unbounded_String; Contents : Unbounded_String) is
   begin
      Put_Paragraph (To_String (Label), To_String (Contents));
   end Put_Paragraph;

   procedure Put_True_False (Truth : String) is
   begin
      Ada.Text_IO.Put ("<img src=""");
      if Truth = "true" then
         Ada.Text_IO.Put ("/icons/tick.png"" alt=""true"" title=""true""");
      elsif Truth = "false" then
         Ada.Text_IO.Put ("/icons/cross.png"" alt=""false"" title=""false""");
      else
         Ada.Text_IO.Put ("/icons/error.png"" alt=""undefined"" title=""undefined""");
      end if;
      Ada.Text_IO.Put_Line (" />");
   end Put_True_False;

   procedure Put_True_False (Truth : Unbounded_String) is
   begin
      Put_True_False (To_String (Truth));
   end Put_True_False;

   procedure Put_Stylesheet (URL : String) is
   begin
      Ada.Text_IO.Put_Line ("<link rel=""stylesheet"" href=""" & URL
                            & """ type=""text/css"" media=""screen"" />");
   end Put_Stylesheet;

   procedure Put_Clearer is
   begin
      Ada.Text_IO.Put_Line ("<div class=""clearer""></div>"); -- css quirk
   end Put_Clearer;

   function Param_Is (Param : String; Expected : String) return Boolean is
   begin
      return Standard."=" (CGI.Value (Param), Expected);
   end Param_Is;

end HTML;
