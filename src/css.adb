with Ada.Text_IO;
with CGI;

package body CSS is

   procedure Put_Body;

   procedure Put_Line (Item : String) renames Ada.Text_IO.Put_Line;

   --------------
   -- Put_Body --
   --------------

   procedure Put_Body is
   begin
      Put_Line ("body {");
      --/*	font-size: 62.5%; */ /* Resets 1em to 10px */
      Put_Line ("  font-family: Times, 'Times New Roman', serif;");
      Put_Line ("  background: #757677;");
      Put_Line ("  color: #222;");
      Put_Line ("  text-align: center;");
      Put_Line ("}");
   end Put_Body;

   procedure Put_Page is
   begin
      Put_Line ("#page {");
      Put_Line ("  padding: 0 0 20px 0;");
      Put_Line ("  width: 75em;");
      Put_Line ("  position: relative;");
      Put_Line ("  text-align: left;");
      Put_Line ("  background: #d5d6d7;");
      Put_Line ("}");
   end Put_Page;

   ---------
   -- Put --
   ---------

   procedure Put is
   begin
      CGI.Put_CGI_Header;
      Put_Body;
      Put_Page;
   end Put;

end CSS;
