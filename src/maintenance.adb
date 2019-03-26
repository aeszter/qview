with HTML;
--  with Parser;
with Ada.Text_IO;
--  with SGE.Hosts; use SGE.Hosts;
--  with SGE.Host_Properties; use SGE.Host_Properties;
--with Hosts; use Hosts;
--  with Host_Properties; use Host_Properties;
--  with SGE.Queues; use SGE.Queues;
--  with Lightsout;
with Ada.Exceptions; use Ada.Exceptions;
--  with SGE.Parser;
--  with SGE.Taint; use SGE.Taint;
--  with SGE.Loggers;
with Ada.Strings;
with Ada.Strings.Fixed;
--  with SGE.Utils;

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
--        SGE_Out : Parser.Tree;
   begin
--        SGE_Out := Parser.Setup (Command => Cmd_Qhost,
--                                 Selector => Implicit_Trust ("-j -F load_short,load_medium, -q"));
--
--        SGE.Hosts.Append_List (SGE.Parser.Get_Elements_By_Tag_Name (SGE_Out, "host"));
--        SGE.Parser.Free;
--        SGE_Out := Parser.Setup (Selector => Implicit_Trust ("-F state -explain E"));
--        Queues.Append_List (SGE.Parser.Get_Elements_By_Tag_Name (SGE_Out, "Queue-List"));
--        SGE.Parser.Free;
--        Read_Lightsout_Information;
--
--        Put_Error_Messages;
--        Put_High_Load_Hosts;
--        Put_Low_Load_Hosts;
--        Put_Swapping_Hosts;
--        Put_Error_Queues;
--        Put_Unusual_Queues;
--        Put_Disabled_Queues;
--        Put_Unreachable_Queues;
--        Put_Offline_Queues;
--        Put_Multi_Queues;
--        Put_No_Queues;
--        Put_Old_Config;
      HTML.Put_Paragraph ("Maintenance.Put_All", "unimplemented");
   end Put_All;

--     procedure Read_Lightsout_Information is
--     begin
--        Lightsout.Clear;
--        Lightsout.Read;
--     exception
--        when E : others => Ada.Text_IO.Put_Line ("<em>Could not process lightsout information.</em>");
--           HTML.Comment (Exception_Message (E));
--     end Read_Lightsout_Information;

   ------------
   -- Put_*  --
   ------------

   procedure Put_High_Load_Hosts is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Overloaded Nodes",
                         Level => 3);
      HTML.Put_Heading (Title => "Load_5 > 1.1 * Used_Slots + 0.1 * Free_Slots"
                        & " and Load_1 > 0.9 * Load_5"
                        & " and Queue State does not contain u",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
      Put_Header;
      --        Hosts.Put_Selected (High_Load'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_High_Load_Hosts;

   procedure Put_Low_Load_Hosts is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Underutilized Nodes",
                         Level => 3);
      HTML.Put_Heading (Title => "Load_5 < 0.9 * Used_Slots and Load_1 < 1.1 * Load_5",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
      Put_Header;
--        Hosts.Put_Selected (Low_Load'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Low_Load_Hosts;

   procedure Put_Swapping_Hosts is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Swapping Nodes",
                         Level => 3);
      HTML.Put_Heading (Title => "Swap_Used > 50%",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
      Put_Header;
--        Hosts.Put_Selected (High_Swap'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Swapping_Hosts;

   procedure Put_Error_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Queues in Error state",
                         Level => 3);
      HTML.Put_Heading (Title => "Queue State contains E",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
--        Queues.Put_Selected (In_Error_State'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Error_Queues;

   procedure Put_Disabled_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Disabled queues",
                         Level => 3);
      HTML.Put_Heading (Title => "Queue_State has d but not u",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
--        Queues.Put_Selected (Reachable_Disabled'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Disabled_Queues;

   procedure Put_Unreachable_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Unreachable queues",
                         Level => 3);
      HTML.Put_Heading (Title => "Queue_State has u but not d",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
--        Queues.Put_Selected (Unreachable_Enabled'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Unreachable_Queues;

   procedure Put_Unusual_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Unusual queues",
                         Level => 3);
      HTML.Put_Heading (Title => "Queue_Type is not BIP",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
--        Queues.Put_Selected (Unusual_Type'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Unusual_Queues;

   procedure Put_Offline_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Offline queues",
                         Level => 3);
      HTML.Put_Heading (Title => "Queue_State has both u and d",
                        Level => 4);
      HTML.Put_Heading (Title => "This includes nodes switched off by lightsout",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
--        Queues.Put_Selected (Unreachable_Disabled'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Offline_Queues;

   procedure Put_Old_Config is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Old config",
                         Level => 3);
      HTML.Put_Heading (Title => "Queue_State has o",
                        Level => 4);
      Ada.Text_IO.Put_Line ("<table>");
--        Queues.Put_Selected (Old_Config'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Old_Config;

   procedure Put_No_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Hosts with no defined queue",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
--        Hosts.Put_Selected (No_Queue'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_No_Queues;

   procedure Put_Multi_Queues is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Hosts with more than one queue",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<table>");
--        Hosts.Put_Selected (Multi_Queue'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "maintenance");
   end Put_Multi_Queues;

   procedure Put_Error_Messages is
      procedure Put_Error (Message : String);

      procedure Put_Error (Message : String) is
         use Ada.Strings.Fixed;

         Job_Start  : constant Natural := Index (Source => Message, Pattern => "of job") + 7;
         Job_End    : constant Natural := Index (Source => Message, Pattern => "'s", From => Job_Start);
         Host_Start : constant Natural := Index (Source => Message, Pattern => "at host") + 8;
         Host_End   : constant Natural := Message'Last + 1;
      begin
         if Job_Start > 0 and then Host_Start > 0 then
            Ada.Text_IO.Put_Line ("<li>" & Message (Message'First .. Job_Start - 1));
            HTML.Put_Link (Text       => Message (Job_Start .. Job_End - 1),
                           Link_Param => "job_id");
            Ada.Text_IO.Put_Line (Message (Job_End .. Host_Start - 1));
            HTML.Put_Link (Text => Message (Host_Start .. Host_End - 1),
                           Link_Param => "host");
            Ada.Text_IO.Put_Line (Message (Host_End .. Message'Last) & "</li>");
         else
            Ada.Text_IO.Put_Line ("<li>" & Message & "</li>");
         end if;
      end Put_Error;

   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Messages",
                         Level => 3);
      Ada.Text_IO.Put_Line ("<ul>");
--        SGE.Loggers.Iterate_Errors (Put_Error'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell ("unimplemented");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</ul>");
      HTML.End_Div (Class => "maintenance");
   end Put_Error_Messages;

   ------------------------
   -- Selector Functions --
   -- Hosts              --
   ------------------------

--     function High_Load (H : Hosts.Host) return Boolean is
--     begin
--        return SGE.Hosts.Get_Load (H) > 1.1 * SGE.Hosts.Get_Used_Slots (H)
--          + 0.1 * SGE.Hosts.Get_Free_Slots (H)
--          and then SGE.Hosts.Get_Load_One (H) > 0.9 * SGE.Hosts.Get_Load (H)
--          and then not SGE.Hosts.Has_Unreachable_Queue (H);
--     exception
--        when Constraint_Error =>
--           HTML.Error (Value (SGE.Hosts.Get_Name (H)) & " has inconsistent load");
--           return False;
--        when E : SGE.Utils.Operator_Error =>
--           HTML.Error (Value (SGE.Hosts.Get_Name (H)) & " has " & Exception_Message (E));
--           return False;
--     end High_Load;
--
--     function Low_Load (H : Hosts.Host) return Boolean is
--     begin
--        return SGE.Hosts.Get_Load (H) < 0.9  * SGE.Hosts.Get_Used_Slots (H)
--          and then (SGE.Hosts.Get_Load_One (H) < 1.1 * SGE.Hosts.Get_Load (H)
--                    or else SGE.Hosts.Get_Load (H) < 0.1);
--     end Low_Load;
--
--     function High_Swap (H : Hosts.Host) return Boolean is
--     begin
--        return SGE.Hosts.Swap_Percentage (H) > 50;
--     exception
--        when Constraint_Error =>
--           return False; -- heuristic: host has no swap
--     end High_Swap;
--
--     function No_Queue (H : Hosts.Host) return Boolean is
--     begin
--        return Queue_Count (H) = 0;
--     end No_Queue;
--
--     function Multi_Queue (H : Hosts.Host) return Boolean is
--     begin
--        return Queue_Count (H) > 1;
--     end Multi_Queue;
--
--
--
--     ------------------------
--     -- Selector Functions --
--     -- Queues             --
--     ------------------------
--
--     function Old_Config (Q : Queues.Queue) return Boolean is
--     begin
--        return Has_Old_Config (Q);
--     end Old_Config;
--
--     function In_Error_State (Q : Queues.Queue) return Boolean is
--     begin
--        return Has_Error (Q);
--     end In_Error_State;
--
--     function Reachable_Disabled (Q : Queues.Queue) return Boolean is
--     begin
--        return Has_Disabled (Q) and then not Has_Unreachable (Q);
--     end Reachable_Disabled;
--
--     function Unreachable_Enabled (Q : Queues.Queue) return Boolean is
--     begin
--        return Has_Unreachable (Q) and then not Has_Disabled (Q);
--     end Unreachable_Enabled;
--
--     function Unreachable_Disabled (Q : Queues.Queue) return Boolean is
--     begin
--        return Has_Unreachable (Q) and then Has_Disabled (Q);
--     end Unreachable_Disabled;
--
--     function Unusual_Type (Q : Queues.Queue) return Boolean is
--     begin
--        return not (Is_Batch (Q) and then Is_Interactive (Q) and then Is_Parallel (Q));
--     end Unusual_Type;

end Maintenance;
