with HTML;
with Ada.Text_IO;
--  with Host_Properties; use Host_Properties;
--  with Lightsout;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;

with Slurm.Nodes;
use Slurm.Nodes;
with Slurm.Hostlists; use Slurm.Hostlists;
with Slurm.Utils; use Slurm.Utils;
with Nodes;

package body Maintenance is

   function High_Load (N : Nodes.Node) return Boolean is
   begin
      return Slurm.Nodes.Get_Load (N) > 1.1 * Slurm.Nodes.Get_Used_CPUs (N)
        + 0.1 * Slurm.Nodes.Get_Free_CPUs (N)
        and then not Slurm.Nodes.Is_Not_Responding (N);
   exception
      when Constraint_Error =>
         HTML.Error (To_String (Slurm.Nodes.Get_Name (N)) & " has inconsistent load");
         return False;
      when E : Slurm.Utils.Operator_Error =>
         HTML.Error (To_String (Slurm.Nodes.Get_Name (N)) & " has " & Exception_Message (E));
         return False;
   end High_Load;

   function Low_Load (N : Nodes.Node) return Boolean is
   begin
      return Slurm.Nodes.Get_Load (N) < 0.9  * Slurm.Nodes.Get_Used_CPUs (N);
   end Low_Load;

   procedure Put_All is
   begin
--        Read_Lightsout_Information;
--
--        Put_Error_Messages;
      Put_High_Load;
      Put_Low_Load;
--        Put_Swapping_Hosts;
--        Put_Error_Queues;
--        Put_Unusual_Queues;
      Put_Drained;
      Put_Unreachable;
      Put_Offline;
--        Put_Multi_Queues;
--        Put_No_Queues;
--        Put_Old_Config;
   end Put_All;

   procedure Put_Drained is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Drained nodes",
                         Level => 3);
      HTML.Put_Heading (Title => "Node is drained/draining but not unreachable",
                        Level => 4);
      Nodes.Put_Selected (Reachable_Disabled'Access);
      HTML.End_Div (Class => "maintenance");
   end Put_Drained;

   procedure Put_Error_Messages is
      procedure Put_Error (Message : String);

      procedure Put_Error (Message : String) is
         use Ada.Strings.Fixed;

         Job_Start  : constant Natural := Index (Source  => Message,
                                                 Pattern => "of job") + 7;
         Job_End    : constant Natural := Index (Source => Message, Pattern => "'s",
                                                 From   => Job_Start);
         Host_Start : constant Natural := Index (Source  => Message,
                                                 Pattern => "at host") + 8;
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

   procedure Put_High_Load is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Overloaded Nodes",
                         Level => 3);
      HTML.Put_Heading (Title => "Load > 1.1 * Used_CPUs + 0.1 * Free_CPUs"
                        & " and Node is not unreachable",
                        Level => 4);
      Nodes.Put_Selected (High_Load'Access);
      HTML.End_Div (Class => "maintenance");
   end Put_High_Load;

   procedure Put_Low_Load is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Underutilized Nodes",
                         Level => 3);
      HTML.Put_Heading (Title => "Load < 0.9 * Used_CPUs",
                        Level => 4);
      Nodes.Put_Selected (Low_Load'Access);
      HTML.End_Div (Class => "maintenance");
   end Put_Low_Load;

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

   procedure Put_Offline is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Offline queues",
                         Level => 3);
      HTML.Put_Heading (Title => "Node is both unreachable and drained",
                        Level => 4);
      HTML.Put_Heading (Title => "This includes nodes switched off by lightsout",
                        Level => 4);
      Nodes.Put_Selected (Unreachable_Disabled'Access);
      HTML.End_Div (Class => "maintenance");
   end Put_Offline;

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

   procedure Put_Unreachable is
   begin
      HTML.Begin_Div (Class => "maintenance");
      HTML.Put_Heading  (Title => "Unreachable nodes",
                         Level => 3);
      HTML.Put_Heading (Title => "Node is unreachable but not drained",
                        Level => 4);
      Nodes.Put_Selected (Unreachable_Enabled'Access);
      HTML.End_Div (Class => "maintenance");
   end Put_Unreachable;

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

   ------------------------
   -- Selector Functions --
   ------------------------

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
   function Reachable_Disabled (N : Nodes.Node) return Boolean is
   begin
      return Is_Draining (N) and then not Is_Not_Responding (N);
   end Reachable_Disabled;

   function Unreachable_Disabled (N : Nodes.Node) return Boolean is
   begin
      return Is_Not_Responding (N) and then Is_Draining (N);
   end Unreachable_Disabled;

   function Unreachable_Enabled (N : Nodes.Node) return Boolean is
   begin
      return Is_Not_Responding (N) and then not Is_Draining (N);
   end Unreachable_Enabled;

   --
--     function Unusual_Type (Q : Queues.Queue) return Boolean is
--     begin
--        return not (Is_Batch (Q) and then Is_Interactive (Q) and then Is_Parallel (Q));
--     end Unusual_Type;

end Maintenance;
