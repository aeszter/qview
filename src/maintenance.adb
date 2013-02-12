with HTML;
with Parser;
with Ada.Text_IO;
with Hosts;

package body Maintenance is

   -------------
   -- Put_All --
   -------------

   procedure Put_Header is
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Header_Cell ("Node");
      HTML.Put_Header_Cell ("Slots");
      HTML.Put_Header_Cell ("Occupied");
      HTML.Put_Header_Cell ("Load");
      HTML.Put_Header_Cell ("%");
      Ada.Text_IO.Put ("</tr>");
   end Put_Header;

   procedure Put_All is
      SGE_Out : Parser.Tree;
   begin
      --  Generated stub: replace with real body!
      HTML.Put_Heading  (Title => "Overloaded Nodes",
                         Level => 2);
      SGE_Out := Parser.Setup (Command => "qhost", Selector => "-j");

      Hosts.Append_List (Parser.Get_Elements_By_Tag_Name (SGE_Out, "Host"));
      Parser.Free;


      HTML.Begin_Div (Class => "partitions");
      Ada.Text_IO.Put_Line ("<table>");
      Put_Header;
      Hosts.Put_List;
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "partitions");

   end Put_All;

end Maintenance;
