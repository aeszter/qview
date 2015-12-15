with Ada.Text_IO;
with CGI;
with HTML;
with SGE.Bunches; use SGE.Bunches;
with SGE.Jobs; use SGE.Jobs;
with Jobs;

package body Bunches is

   List : SGE.Bunches.List;
   ----------------
   -- Build_List --
   ----------------

   procedure Build_List is
   begin
      Jobs.Bunch (List);
   end Build_List;


   procedure Put_List is
   begin
      Ada.Text_IO.Put_Line ("<table><tr>");
      HTML.Put_Cell (Data => "<acronym title=""click on arrow to view job list"">"
                     & "Detail</acronym>", Tag => "th");
      HTML.Put_Cell (Data => "<acronym title=""Parallel Environment"">PE</acronym>",
                     Tag => "th");
      HTML.Put_Cell (Data => "CPU Slots", Tag => "th");
      for Capability in Balancer_Capability loop
         if Capability /= Any then
            HTML.Put_Cell (Data => "", Tag => "th");
         end if;
      end loop;
      HTML.Put_Cell (Data => "GPU Slots", Tag => "th");
      HTML.Put_Cell (Data => "Queue", Tag => "th");
      HTML.Put_Cell (Data => "Hard Requests", Tag => "th");
      HTML.Put_Cell (Data => "Soft Requests", Tag => "th");
      HTML.Put_Cell (Data => "Total", Tag => "th");
      HTML.Put_Cell (Data => "Waiting", Tag => "th");
      HTML.Put_Cell (Data => "Quota", Tag => "th");
      HTML.Put_Cell (Data => "Held", Tag => "th");
      HTML.Put_Cell (Data => "Error", Tag => "th");
      Ada.Text_IO.Put_Line ("</tr>");
      SGE.Bunches.Iterate (List, Put'Access);
   end Put_List;

   ---------
   -- Put --
   --  Purpose: Output one Bunch of Jobs as a single line in a table.
   --  Parameter Pos : Cursor into a List of Bunches, signifies the Bunch to be output
   ---------

   procedure Put (B : SGE.Bunches.Bunch) is
      use SGE.Jobs;

   begin
      if Has_Error (B) then
         Ada.Text_IO.Put ("<tr class=""job-error"">");
      elsif not Has_Waiting (B) then
         Ada.Text_IO.Put ("<tr class=""job-held"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;

      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?jobs=bunch"
               & "&pe=" & Get_PE (B)
               & "&slot_ranges=" & Get_Slot_Hash (B)
               & "&slot_number=" & Get_Slot_Number (B)
               & "&queue=" & Get_Queue (B)
               & "&hr=" & Get_Hard_Hash (B)
               & "&sr=" & Get_Soft_Hash (B)
               & """><img src=""/icons/arrow_right.png"" /></a>");

      if Get_PE (B) = "" then
         HTML.Put_Cell ("(none)");
      else
         HTML.Put_Cell (Data => Get_PE (B));
      end if;
      HTML.Put_Cell (Data  => Get_CPU_Slot_Numbers (B),
                     Class => (if Is_Queued_For_GPU (B) then "right"
                               else "right_active"));
      for Capability in Balancer_Capability'Range loop
         if Capability /= Any then
            if Has_Balancer (B, Capability) then
               HTML.Put_Img_Cell ("balance_" & To_String (Capability));
            else
               HTML.Put_Cell ("");
            end if;
         end if;
      end loop;
      HTML.Put_Cell (Data  => Get_GPU_Slot_Numbers (B),
                     Class => (if Is_Queued_For_GPU (B) then "right_active"
                               else "right"));
      HTML.Put_Cell (Data => Get_Queue (B));
      HTML.Put_Cell (Data => Get_Hard_Resources (B));
      HTML.Put_Cell (Data => Get_Soft_Resources (B));
      HTML.Put_Cell (Data => Get_Total_Jobs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Waiting_Jobs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Quota_Inhibited_Jobs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Jobs_On_Hold (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Errors (B)'Img, Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put;
end Bunches;
