with HTML;
with Parser;
with Ada.Text_IO;
with Hosts; use Hosts;
with Host_Properties; use Host_Properties;
with Queues; use Queues;

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
      HTML.Put_Header_Cell ("per Core");
      HTML.Put_Header_Cell ("Swap");
      Ada.Text_IO.Put ("</tr>");
   end Put_Header;

   -------------
   -- Put_All --
   -------------

   procedure Put_All is
      SGE_Out : Parser.Tree;
   begin
      SGE_Out := Parser.Setup (Command => "qhost",
                               Selector => "-j -F load_short,load_medium");

      Hosts.Append_List (Parser.Get_Elements_By_Tag_Name (SGE_Out, "host"));
      Parser.Free;
      SGE_Out := Parser.Setup (Selector => "-F state");
      Queues.Append_List (Parser.Get_Elements_By_Tag_Name (SGE_Out, "Queue-List"));
      Parser.Free;


      Put_High_Load_Hosts;
      Put_Low_Load_Hosts;
      Put_Swapping_Hosts;
      Put_Error_Queues;
      Put_Unusual_Queues;
      Put_Disabled_Queues;
      Put_Unreachable_Queues;
   end Put_All;

   ------------
   -- Put_*  --
   ------------

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

   procedure Put_Error_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Queues in Error state",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
      Queues.Put_Selected (In_Error_State'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Error_Queues;

   procedure Put_Disabled_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Disabled queues",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
      Queues.Put_Selected (Reachable_Disabled'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Disabled_Queues;

   procedure Put_Unreachable_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Unreachable queues",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
      Queues.Put_Selected (Unreachable_Enabled'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Unreachable_Queues;

   procedure Put_Unusual_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Unusual queues",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
      Queues.Put_Selected (Unusual_Type'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Unusual_Queues;


   ------------------------
   -- Selector Functions --
   -- Hosts              --
   ------------------------

   function High_Load (H : Hosts.Host) return Boolean is
   begin
      return Hosts.Get_Load (H) > 1.1 * Hosts.Get_Used_Slots (H)
        + 0.1 * Hosts.Get_Free_Slots (H)
        and then Hosts.Get_Load_One (H) > 1.0 * Hosts.Get_Used_Slots (H)
        + 0.15 * Hosts.Get_Free_Slots (H);
   end High_Load;

   function Low_Load (H : Hosts.Host) return Boolean is
   begin
      return Hosts.Get_Load (H) < 0.9  * Hosts.Get_Used_Slots (H)
      and then Hosts.Get_Load_One (H) < 0.9 * Hosts.Get_Used_Slots (H);
   end Low_Load;

   function High_Swap (H : Hosts.Host) return Boolean is
   begin
      return Hosts.Swap_Percentage (H) > 50;
   exception
      when Constraint_Error =>
         return False; -- heuristic: host has no swap
   end High_Swap;

   ------------------------
   -- Selector Functions --
   -- Queues             --
   ------------------------

   function In_Error_State (Q : Queues.Queue) return Boolean is
   begin
      return Has_Error (Q);
   end In_Error_State;

   function Reachable_Disabled (Q : Queues.Queue) return Boolean is
   begin
      return Has_Disabled (Q) and then not Has_Unreachable (Q);
   end Reachable_Disabled;

   function Unreachable_Enabled (Q : Queues.Queue) return Boolean is
   begin
      return Has_Unreachable (Q) and then not Has_Disabled (Q);
   end Unreachable_Enabled;

   function Unusual_Type (Q : Queues.Queue) return Boolean is
   begin
      return not (Is_Batch (Q) and then Is_Interactive (Q) and then Is_Parallel (Q));
   end Unusual_Type;

end Maintenance;
