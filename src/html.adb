with Ada.Text_IO; use Ada.Text_IO;
with CGI;

package body HTML is

   --------------
   -- Put_Cell --
   --  Purpose: Write a table cell with given contents.
   --  Input: Data: Contents of the cell;
   --  Input: Tag: (optional) tag to use instead of <td> (e.g. <th>)
   --------------

   procedure Put_Cell (Data : String; Tag : String := "td") is
   begin
      Put_Line ("<" & Tag & ">" & Data & "</" & Tag & ">");
   end Put_Cell;

   --------------
   -- Put_UCell --
   --  Purpose: Write a table cell with given contents.
   --  Input: Data: Contents of the cell;
   --------------

   procedure Put_UCell (Data : Unbounded_String) is
   begin
      Put_Cell (To_String (Data));
   end Put_UCell;

   --------------
   -- Put_UCell_With_Link --
   --  Purpose: Write a table cell with given contents, linking to .
   --  Input: Data: Contents of the cell;
   --  Input: Link_Param: CGI Parameter to use in link, sc.
   --        <a href="the_url?Link_Param=Data">Data</a>
   --------------

   procedure Put_UCell_With_Link (Data : Unbounded_String; Link_Param : String) is
      S : String := To_String (Data);
   begin
      Put_Cell (Data => "<a href=""" & CGI.My_URL & "?"
                & Link_Param & "=" & S & """>" & S & "</a>");
   end Put_UCell_With_Link;

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

   procedure Put_Paragraph (Label : String; Contents : Unbounded_string) is
   begin
      Put_Paragraph (Label, To_String (Contents));
   end Put_Paragraph;

   procedure Put_True_False (Truth : String) is
   begin
      Ada.Text_IO.Put ("<img src=""");
      if Truth = "true" then
         Ada.Text_IO.Put ("/icons/tick.png"" alt=""true""");
      elsif Truth = "false" then
         Ada.Text_IO.Put ("/icons/cross.png"" alt=""false""");
      else
         Ada.Text_IO.Put ("/icons/error.png"" alt=""undefined""");
      end if;
      Ada.Text_IO.Put_Line (" />");
   end Put_True_False;

   procedure Put_True_False (Truth : Unbounded_String) is
   begin
      Put_True_False (To_String (Truth));
   end Put_True_False;

end HTML;
