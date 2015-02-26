with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with HTML;
with Lightsout;
with Ada.Strings.Fixed;
with Ada.Calendar; use Ada.Calendar;
with SGE.Utils;

package body Hosts is

--   use Host_Lists;
--   use Job_Lists;
--   use Queue_Maps;

   procedure Put_All is
   begin
      HTML.Put_Heading (Title => "Hosts " & HTML.Help_Icon (Topic => "Host List"),
                        Level => 2);
      HTML.Begin_Div (Class => "host_list");
      Ada.Text_IO.Put_Line ("<table><tr>");
      HTML.Put_Header_Cell (Data     => "Name");
      HTML.Put_Header_Cell (Data     => "Interconnect");
      HTML.Put_Header_Cell (Data => "GPU");
      HTML.Put_Cell (Data     => "CPU" & HTML.Help_Icon (Topic => "CPU Families"),
                    Tag => "th");
      HTML.Put_Header_Cell (Data     => "Cores");
      HTML.Put_Header_Cell (Data     => "Free");
      HTML.Put_Header_Cell (Data     => "Reserved");
      HTML.Put_Header_Cell (Data     => "RAM");
      HTML.Put_Header_Cell (Data     => "Load",
                           Acronym => "per core");
      HTML.Put_Header_Cell (Data => "Mem",
                            Acronym => "% used");
      HTML.Put_Header_Cell (Data => "Swap",
                            Acronym => "% used");
      HTML.Put_Header_Cell (Data     => "Queues",
                            Sortable => False);
      Ada.Text_IO.Put ("</tr>");
      Lightsout.Clear;
      Lightsout.Read;
      SGE.Hosts.Iterate (Hosts.Put'Access);
      --  Table Footer
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "host_list");

   end Put_All;

   procedure Put_Details is
   begin
      Lightsout.Clear;
      Lightsout.Read;
      SGE.Hosts.Iterate (Put_Details'Access);
   end Put_Details;

   procedure Put_Details (H : SGE.Hosts.Host) is
      procedure Put_Actions;
      procedure Put_Name;
      procedure Put_Properties;
      procedure Put_Queues;
      procedure Put_Resources;
      procedure Put_Jobs;
      procedure Put_Queue_Row (Q : Queue_Pointer);
      procedure Put_Job (J : Job);

      procedure Put_Actions is
      begin
         HTML.Begin_Div (ID => "host_actions");
         HTML.Put_Img (Name => "maint_no",
                       Text => "Clear maintenance",
                       Link => HTML.Get_Action_URL (Action => "cm",
                                                    Params => "h=" & Get_Name (H)));
         HTML.Put_Img (Name => "maint_ignore",
                       Text => "Make lightsout ignore node",
                       Link => HTML.Get_Action_URL (Action => "im",
                                                    Params => "h=" & Get_Name (H)));
         HTML.Put_Img (Name => "maint_disable",
                       Text => "Disable node",
                       Link => HTML.Get_Action_URL (Action => "dm",
                                                    Params => "h=" & Get_Name (H)));
         HTML.Put_Img (Name => "maint_off",
                       Text => "Mark node for poweroff",
                       Link => HTML.Get_Action_URL (Action => "om",
                                                    Params => "h=" & Get_Name (H)));
         HTML.Put_Paragraph (Label    => "Missing",
                             Contents => "Enter bug id");
         HTML.End_Div (ID => "host_actions");
         HTML.Begin_Div (ID => "unlocker");
         HTML.Put_Img (Name => "hand.right",
                       Text => "unlock host manipulation",
                       Link => "#",
                       Extra_Args => "onclick=""document.getElementById('host_actions').style.display = 'block';"
                                 & "document.getElementById('unlocker').style.display = 'none' """);
         HTML.End_Div (ID => "unlocker");
      end Put_Actions;

      procedure Put_Name is
      begin
         HTML.Begin_Div (Class => "host_name");
         Ada.Text_IO.Put ("<p>");

         Ada.Text_IO.Put_Line (Get_Name (H) & "</p>");
         Ada.Text_IO.Put_Line ("<p>" & Lightsout.Get_Maintenance (Get_Name (H)));
         Ada.Text_IO.Put_Line (Lightsout.Get_Bug (Get_Name (H)) & "</p>");
         HTML.End_Div (Class => "host_name");
      end Put_Name;

      procedure Put_Properties is
      begin
         HTML.Begin_Div ("host_properties");
         HTML.Put_Paragraph ("Load", Get_Load_One (H)'Img & "," & Get_Load (H)'Img);
         HTML.Put_Paragraph ("Slots used", Get_Used_Slots (H)'Img);
         HTML.Put_Paragraph ("Memory", Get_Memory (H));
         HTML.Put_Paragraph ("Network", Get_Network (H));
         HTML.Put_Paragraph ("CPU", Get_Model (H));
         HTML.Put_Paragraph ("Cores", Get_Cores (H)'Img);
         HTML.Put_Paragraph ("GPU", Get_GPU (H));
         HTML.End_Div ("host_properties");
      end Put_Properties;

      --  Add table with coloured memory, swap usage cells

      procedure Put_Queue_Row (Q : Queue_Pointer) is
      begin
         Ada.Text_IO. Put ("<tr>");
         Put_Queue (Q);
         Ada.Text_IO. Put ("</tr>");
      end Put_Queue_Row;

      procedure Put_Job (J : Job) is
      begin
         Put_Jobs (J);
      end Put_Job;

      procedure Put_Queues is
      begin
         HTML.Begin_Div ("host_queues");
         HTML.Put_Heading ("Queues", 3);
         Ada.Text_IO.Put ("<table>");
         Iterate_Queues (H, Put_Queue_Row'Access);
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div ("host_queues");
      end Put_Queues;

      procedure Put_Jobs is
      begin
         HTML.Begin_Div ("host_jobs");
         HTML.Put_Heading ("Jobs", 3);
         Ada.Text_IO.Put ("<table>");
         Iterate_Jobs (H, Put_Job'Access);
         Ada.Text_IO.Put_Line ("</table>");
         HTML.End_Div ("host_jobs");
      end Put_Jobs;

      procedure Put_Resources is
      begin
         HTML.Begin_Div ("host_res");
         Ada.Text_IO.Put ("<table><tr>");
         HTML.Put_Header_Cell (Data     => "Load",
                               Acronym  => "per core",
                               Sortable => False);
         HTML.Put_Header_Cell (Data     => "Mem",
                               Acronym  => "% used",
                               Sortable => False);
         HTML.Put_Header_Cell (Data     => "Swap",
                               Acronym  => "% used",
                               Sortable => False);
         Ada.Text_IO.Put ("</tr><tr>");
         HTML.Put_Cell (Data  => Load_Per_Core (H)'Img,
                        Class => "right " & Color_Class (Load_Per_Core (H)));
         HTML.Put_Cell (Data  => Mem_Percentage (H)'Img,
                        Class => "right " & Color_Class (Mem_Percentage (H)));
         HTML.Put_Cell (Data  => Swap_Percentage (H)'Img,
                        Class => "right " & Color_Class (Swap_Percentage (H)));
         Ada.Text_IO.Put ("</tr></table>");
         HTML.End_Div ("host_res");
      end Put_Resources;

   begin
      HTML.Begin_Div (Class => "host_info");

      HTML.Begin_Div (Class => "action_and_name");
      Put_Actions;
      Put_Name;
      HTML.Put_Clearer;
      HTML.End_Div (Class => "action_and_name");
      Put_Properties;
      Put_Queues;
      Put_Jobs;
      Put_Resources;
      HTML.Put_Clearer;
      HTML.End_Div (Class => "host_info");
      HTML.Put_Clearer;
   end Put_Details;

   procedure Put_Selected (Selector : not null access function (H : Host) return Boolean) is
   begin
      SGE.Hosts.Iterate (Process  => Put_For_Maintenance'Access,
                         Selector => Selector);
   end Put_Selected;

   ---------
   -- Put --
   --  Purpose : Output one Host as an HTML <tr>,
   --    adding one <tr> for every Job on the Host
   --  Parameter Pos : Cursor pointing to the Job record to output
   ---------

   ---------
   -- Put --
   ---------

   procedure Put (H : Host) is
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => Get_Name (H));
      HTML.Put_Cell (Data => Get_Network (H));
      HTML.Put_Cell (Data => Get_GPU (H));
      HTML.Put_Cell (Data => Get_Model (H));
      HTML.Put_Cell (Data => Get_Cores (H)'Img, Class => "right");
      begin
         HTML.Put_Cell (Data => Get_Free_Slots (H)'Img, Class => "right");
      exception
         when E : SGE.Utils.Operator_Error =>
            HTML.Put_Cell (Data => "err", Class => "right", Acronym => Exception_Message (E));
      end;
      HTML.Put_Cell (Data => Get_Reserved_Slots (H)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Memory (H), Class => "right");
      HTML.Put_Cell (Data  => Load_Per_Core (H)'Img,
                     Class => "right " & Color_Class (Load_Per_Core (H)));
      HTML.Put_Cell (Data  => Mem_Percentage (H)'Img,
                     Class => "right " & Color_Class (Mem_Percentage (H)));
      HTML.Put_Cell (Data  => Swap_Percentage (H)'Img,
                     Class => "right " & Color_Class (Swap_Percentage (H)));
      Iterate_Queues (H, Put_Queue'Access);
      HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Get_Name (H)));
      HTML.Put_Cell (Data => Lightsout.Get_Bug (Get_Name (H)), Class => "right");
      Ada.Text_IO.Put ("</tr>");
      Iterate_Jobs (H, Put_Jobs'Access);
   exception
      when E : others =>
         HTML.Error ("Error while putting host " & Get_Name (H) & ": "
                     & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put;

   procedure Put_For_Maintenance (H : Host) is
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => Get_Name (H));
      HTML.Put_Cell (Data => Get_Cores (H)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Used_Slots (H)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Load (H)'Img, Class      => "right");
      HTML.Put_Cell (Data  => Load_Per_Core (H)'Img,
                     Class => "right " & Color_Class (Load_Per_Core (H)));
      HTML.Put_Cell (Data  => Swap_Percentage (H)'Img,
                     Class => "right " & Color_Class (Swap_Percentage (H)));
      Iterate_Queues (H, Put_Queue'Access);
      HTML.Put_Cell (Data => Lightsout.Get_Maintenance (Get_Name (H)));
      HTML.Put_Cell (Data => Lightsout.Get_Bug (Get_Name (H)), Class => "right");
      Ada.Text_IO.Put ("</tr>");
      Iterate_Jobs (H, Put_Jobs'Access);
   exception
      when E : others =>
         HTML.Error ("Error while putting host " & Get_Name (H) & ": "
                     & Exception_Message (E));
         Ada.Text_IO.Put ("</tr>");
   end Put_For_Maintenance;

   --------------
   -- Put_Jobs --
   --------------

   procedure Put_Jobs (J : Job) is
   begin
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => ""); -- H.Name
      if Is_Master (J) then
         if Has_Slaves (J) then -- master
            HTML.Put_Cell (Data => "<img src=""/icons/master.png"" />" & Get_Slaves (J)'Img,
                           Class => "right");
         else -- serial
            HTML.Put_Cell (Data => "<img src=""/icons/serial.png"" />" & "1",
                           Class => "right");
         end if;
      else
         HTML.Put_Cell (Data => Get_Slaves (J)'Img, Class => "right");
      end if;
      HTML.Put_Cell (Data => Ada.Strings.Fixed.Trim (Get_ID (J)'Img, Ada.Strings.Left),
                    Link_Param => "job_id");
      HTML.Put_Duration_Cell (Ada.Calendar.Clock - Get_Start_Time (J));
      Ada.Text_IO.Put ("</tr>");
   end Put_Jobs;

   ----------------
   -- Put_Status --
   ----------------

   procedure Put_Queue (Q : Queue_Pointer) is
   begin
      HTML.Put_Cell (Data => Get_Name (Q) & ':'
                     & Get_Slots (Q)'Img);
      HTML.Put_Img_Cell (Image => Get_State (Q));
   end Put_Queue;

end Hosts;
