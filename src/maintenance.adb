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
      SGE_Out := Parser.Setup (Command => "qhost", Selector => "-j");

      Hosts.Append_List (Parser.Get_Elements_By_Tag_Name (SGE_Out, "host"));
      Parser.Free;

      Put_High_Load_Hosts;
      Put_Low_Load_Hosts;
      Put_Swapping_Hosts;
   end Put_All;

   procedure Put_High_Load_Hosts is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Overloaded Nodes",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
      Put_Header;
      Hosts.Put_Selected (High_Load'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_High_Load_Hosts;

   procedure Put_Low_Load_Hosts is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Underutilized Nodes",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
      Put_Header;
      Hosts.Put_Selected (Low_Load'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Low_Load_Hosts;

   procedure Put_Swapping_Hosts is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Swapping Nodes",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
      Put_Header;
      Hosts.Put_Selected (High_Swap'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Swapping_Hosts;

   function High_Load (H : Hosts.Host) return Boolean is
   begin
      return ;
   end High_Load;

   function Low_Load (H : Hosts.Host) return Boolean is
   begin
      return False;
   end Low_Load;

   function High_Swap (H : Hosts.Host) return Boolean is
   begin
      return Swap_Percent (H) > 50;
   end High_Swap;

end Maintenance;
