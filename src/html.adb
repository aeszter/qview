with Ada.Text_IO; use Ada.Text_IO;
with CGI;

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
                       Tag        : String := "td") is
   begin
      if Link_Param = "" then
         Put_Line ("<" & Tag & ">" & Data & "</" & Tag & ">");
      else
         Put_Line ("<" & Tag & "><a href=""" & CGI.My_URL & "?"
                   & Link_Param & "=" & Data & """>"
                   & Data & "</a></" & Tag & ">");
      end if;
   end Put_Cell;

   procedure Put_Cell (Data       : Unbounded_String;
                       Link_Param : String := "";
                       Tag        : String := "td") is
   begin
      Put_Cell (Data       => To_String (Data),
                Link_Param => Link_Param,
                Tag        => Tag);
   end Put_Cell;

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
                   Tag        => "th");
      else -- Sortable and Params
         Put_Cell (Data       => Data,
                   Link_Param => To_String (Params & "&sort"),
                   Tag        => "th");
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

   procedure Put_Paragraph (Label : String; Contents : Unbounded_string) is
   begin
      Put_Paragraph (Label, To_String (Contents));
   end Put_Paragraph;

   procedure Put_Paragraph (Label : Unbounded_String; Contents : Unbounded_string) is
   begin
      Put_Paragraph (To_String (Label), To_String (Contents));
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

   procedure Put_Stylesheet (URL : String) is
   begin
      Ada.Text_IO.Put_Line ("<link rel=""stylesheet"" href=""" & URL
                            & """ type=""text/css"" media=""screen"" />");
   end Put_Stylesheet;

   procedure Put_Clearer is
   begin
      Ada.Text_IO.Put_Line ("<div class=""clearer""></div>"); -- css quirk
   end Put_Clearer;


end HTML;
