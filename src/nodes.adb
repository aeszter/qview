with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
--  with Lightsout;
with Slurm.Jobs; use Slurm.Jobs;
with Slurm.Partitions; use Slurm.Partitions;
with Slurm.Gres;
with Slurm.Utils;
with HTML;
with Utils;

package body Nodes is

--   use Host_Lists;
--   use Job_Lists;
--   use Queue_Maps;

   procedure Put (Position : Slurm.Nodes.Cursor);
   procedure Put_GPU_Cell (N : Node);
   procedure Put_State (N : Node);

--
--     procedure Put_Selected (Selector : not null access function (H : Host) return Boolean) is
--     begin
--        SGE.Hosts.Iterate (Process  => Put_For_Maintenance'Access,
--                           Selector => Selector);
--     end Put_Selected;
--
--     ---------
--     -- Put --
--     --  Purpose : Output one Host as an HTML <tr>,
--     --    adding one <tr> for every Job on the Host
--     --  Parameter Pos : Cursor pointing to the Job record to output
--     ---------
--
--     ---------
--     -- Put --
--     ---------
--
   function Explain_State (S : Slurm.Nodes.states) return String is
   begin
      case S is
         when NODE_STATE_ALLOCATED =>
            return "allocated to a job";
         when NODE_STATE_DOWN =>
            return "down";
         when NODE_STATE_ERROR =>
            return "error";
         when NODE_STATE_FUTURE =>
            return "reserved for future use";
         when NODE_STATE_IDLE =>
            return "idle";
         when NODE_STATE_MIXED =>
            return "mixed";
         when NODE_STATE_UNKNOWN =>
            return "unknown";
      end case;
   end Explain_State;

   procedure Put (Position : Slurm.Nodes.Cursor) is
      N : Node;
   begin
      if Has_Element (Position) then
         N := Element (Position);
      else
         raise Constraint_Error with "no such node";
      end if;
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Img_Cell (Get_State (N));
      HTML.Put_Cell (Data => Get_Name (N),
                    Link_Param => "node");
      --        HTML.Put_Cell (Data => Get_Network (H));
      HTML.Put_Cell ("");
      Put_GPU_Cell (N);
      --        HTML.Put_Cell (Data => To_String (Get_Model (H)));
      HTML.Put_Cell ("");
      HTML.Put_Cell (Data => Get_CPUs (N)'Img, Class => "right");
--        begin
--           HTML.Put_Cell (Data => Get_Free_Slots (H)'Img, Class => "right");
--        exception
--           when E : SGE.Utils.Operator_Error =>
--              HTML.Put_Cell (Data => "err", Class => "right", Acronym => Exception_Message (E));
--        end;
      HTML.Put_Cell ("");
      HTML.Put_Cell (Data => Get_Memory (N), Class => "right");
      HTML.Put_Cell (Data  => Load_Per_Core (N)'Img,
                     Class => "right " & Color_Class (Load_Per_Core (N)));
      HTML.Put_Cell (Data  => Mem_Percentage (N)'Img,
                     Class => "right " & Color_Class (Mem_Percentage (N)));
      Iterate_Partitions (N, Put_Partition'Access);
--        HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Get_Name (H)));
--        HTML.Put_Cell (Data => Lightsout.Get_Bug (Get_Name (H)), Class => "right");
      Ada.Text_IO.Put ("</tr>");
      Iterate_Jobs (N, Put_Jobs'Access);
   exception
      when --  E :
         others =>
         --           HTML.Error ("Error while putting host "
--           & SGE.Host_Properties.Value (Get_Name (H)) & ": "
--                       & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put;

   procedure Put_All is
   begin
      Put_List (Slurm.Nodes.Load_Nodes);
   end Put_All;

   procedure Put_Details (Name : String) is
      procedure Put_Hardware;
      procedure Put_Resources;
      procedure Put_Slurm;
      procedure Put_System;

      The_List : constant Slurm.Nodes.List := Slurm.Nodes.Load_Nodes;
      N        : constant Node := Slurm.Nodes.Get_Node (The_List, Name);

      procedure Put_Hardware is
      begin
         HTML.Begin_Div (Class => "node_hardware");
         HTML.Put_Paragraph ("Architecture", Get_Architecture (N));
         HTML.Put_Paragraph ("Boards",  Get_Boards (N)'Img);
         HTML.Put_Paragraph ("Sockets:Cores:Threads",
                             Utils.To_String (Get_Sockets (N)) & ":"
                             & Utils.To_String (Get_Cores_Per_Socket (N)) & ":"
                             & Utils.To_String (Get_Threads_Per_Core (N)));
         HTML.Put_Paragraph ("CPUs", Get_CPUs (N)'Img);
         HTML.Put_Clearer;
         HTML.End_Div (Class => "node_hardware");
      end Put_Hardware;

      procedure Put_Resources is
         procedure Put_GRES (Res : Slurm.Gres.Resource);

         Label : String := "GRES ";

         procedure Put_GRES (Res : Slurm.Gres.Resource) is
            use Slurm.Gres;
         begin
            HTML.Put_Paragraph (Label => Label, Contents => To_String (Res));
         end Put_GRES;

      begin
         HTML.Begin_Div (Class => "node_resources");
         HTML.Put_Paragraph ("Load per core", Load_Per_Core (N)'Img);
         HTML.Put_Paragraph ("Memory free/total", Get_Free_Memory (N) & "/" & Get_Memory (N));
         HTML.Put_Paragraph ("Features", Get_Features (N));

         Iterate_GRES (N, Put_GRES'Access);
         Label := "Drain";
         Iterate_GRES_Drain (N, Put_GRES'Access);
         Label := "Used ";
         Iterate_GRES_Used (N, Put_GRES'Access);

         HTML.Put_Paragraph ("tmp", Slurm.Utils.To_String (Get_Tmp_Total (N)));
         HTML.Put_Paragraph ("TRES", Get_TRES (N));
         HTML.Put_Clearer;
         HTML. End_Div (Class => "node_resources");
      exception
            when others =>
         HTML.Put_Clearer;
         HTML. End_Div (Class => "node_resources");
      end Put_Resources;

      procedure Put_Slurm is
         use Slurm.Utils;
      begin
         HTML.Begin_Div (Class => "node_slurm");
         HTML.Put_Paragraph ("Owner", To_String (Get_Owner (N)));
         Ada.Text_IO.Put ("<p>State: ");
         Put_State (N);
         if Get_Reason (N) /= "" then
            HTML.Put_Paragraph ("Reason", Get_Reason (N));
            HTML.Put_Paragraph ("by", To_String (Get_Reason_User (N)));
            HTML.Put_Paragraph ("since", Get_Reason_Time (N));
         end if;
         Ada.Text_IO.Put_Line ("</p>");
         HTML.Put_Paragraph ("Job started", Get_Start_Time (N));
         HTML.Put_Paragraph ("Weight", Get_Weight (N)'Img);
         HTML.Put_Paragraph ("Version", Get_Version (N));
         HTML.Put_Clearer;
         HTML.End_Div (Class => "node_slurm");
      end Put_Slurm;

      procedure Put_System is
      begin
         HTML.Begin_Div (Class => "node_system");
         Ada.Text_IO. Put_Line ("<p>" & Get_Name (N) & "</p>");
         Ada.Text_IO.Put_Line ("<p class=""message"">" & Get_OS (N) & "</p>");
         Ada.Text_IO.Put_Line ("<p class=""message"">Booted: "
                               & HTML.To_String (Get_Boot_Time (N)) & "</p>");
         HTML.Put_Clearer;
         HTML. End_Div (Class => "node_system");
      end Put_System;

   begin
      HTML.Begin_Div (Class => "node_info");
      HTML.Begin_Div (Class => "node_head_data");
      Put_System;
      HTML.End_Div (Class => "node_head_data");
      Put_Hardware;
      Put_Slurm;
      Put_Resources;
      HTML.Put_Clearer;
      HTML. End_Div (Class => "node_info");
   end Put_Details;

   procedure Put_GPU_Cell (N : Node) is
      use Slurm.Gres;
      use Ada.Strings.Unbounded;

      procedure Put_GPU (R : Resource);

      procedure Put_GPU (R : Resource) is
      begin
         if R.Category = "gpu" or else
           R.Category = "GPU"
         then
            Ada.Text_IO.Put_Line (R.Number'Img & " " & To_String (R.Name));
         end if;
      end Put_GPU;

   begin
      Ada.Text_IO.Put ("<td>");
      Iterate_GRES (N, Put_GPU'Access);
      Ada.Text_IO.Put ("</td>");
   end Put_GPU_Cell;

   procedure Put_Jobs (J : Slurm.Jobs.Job) is
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => ""); -- H.Name
      HTML.Put_Cell (Data => Ada.Strings.Fixed.Trim (Get_ID (J)'Img, Ada.Strings.Left),
                    Link_Param => "job_id");
      HTML.Put_Duration_Cell (Ada.Calendar.Clock - Get_Start_Time (J));
      Ada.Text_IO.Put ("</tr>");
   end Put_Jobs;

   procedure Put_List (List : Slurm.Nodes.List) is
   begin
      HTML.Put_Heading (Title => "Nodes " & HTML.Help_Icon (Topic => "Node List"),
                        Level => 2);
      HTML.Begin_Div (Class => "host_list");
      Ada.Text_IO.Put_Line ("<table><tr>");
      HTML.Put_Header_Cell (Data     => "State");
      HTML.Put_Header_Cell (Data     => "Name");
      HTML.Put_Header_Cell (Data     => "Interconnect");
      HTML.Put_Header_Cell (Data => "GPU");
      HTML.Put_Cell (Data     => "CPU" & HTML.Help_Icon (Topic => "CPU Families"),
                    Tag => "th");
      HTML.Put_Header_Cell (Data     => "Cores");
      HTML.Put_Header_Cell (Data     => "Free");
      HTML.Put_Header_Cell (Data     => "RAM");
      HTML.Put_Header_Cell (Data     => "Load",
                           Acronym => "per core");
      HTML.Put_Header_Cell (Data => "Mem",
                            Acronym => "% used");
      HTML.Put_Header_Cell (Data     => "Partitions",
                            Sortable => False);
      Ada.Text_IO.Put ("</tr>");
--        Lightsout.Clear;
--        Lightsout.Read;
      Iterate (List, Put'Access);
      --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "host_list");

   end Put_List;
--
--     procedure Put_For_Maintenance (H : Host) is
--     begin
--        Ada.Text_IO.Put ("<tr>");
--        HTML.Put_Cell (Data => HTML.To_String (Get_Name (H)));
--        HTML.Put_Cell (Data => Get_Cores (H)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Used_Slots (H)'Img, Class => "right");
--        HTML.Put_Cell (Data => Get_Load (H)'Img, Class      => "right");
--        HTML.Put_Cell (Data  => Load_Per_Core (H)'Img,
--                       Class => "right " & Color_Class (Load_Per_Core (H)));
--        HTML.Put_Cell (Data  => Swap_Percentage (H)'Img,
--                       Class => "right " & Color_Class (Swap_Percentage (H)));
--        Iterate_Queues (H, Put_Queue'Access);
--        HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Get_Name (H)));
--        HTML.Put_Cell (Data => Lightsout.Get_Bug (Get_Name (H)), Class => "right");
--        Ada.Text_IO.Put ("</tr>");
--        Iterate_Jobs (H, Put_Jobs'Access);
--     exception
--        when E : others =>
--           HTML.Error ("Error while putting host "
-- & SGE.Host_Properties.Value (Get_Name (H)) & ": "
--                       & Exception_Message (E));
--           Ada.Text_IO.Put ("</tr>");
--     end Put_For_Maintenance;
--
   procedure Put_Partition (P : Slurm.Partitions.Partition) is
   begin
      HTML.Put_Cell (Data => Get_Name (P));
--      HTML.Put_Img_Cell (Image => Get_State (P));
   end Put_Partition;

   procedure Put_State (N : Node) is
      procedure Put (What : String) renames Ada.Text_IO.put;
   begin
      Put ("<img src=""/icons/" & Get_State (N) & ".png"" ");
      Put ("alt=""" & Get_State (N) & """ title=""" & Get_State (N) & ": ");
      Put (Explain_State (Get_State (N)));
      Put (""" />");
   end Put_State;

end Nodes;
