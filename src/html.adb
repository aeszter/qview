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
   -- Put_Cell --
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

end HTML;
