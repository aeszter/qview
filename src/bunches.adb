with Ada.Text_IO;
with CGI;
with HTML;
with SGE.Bunches; use SGE.Bunches;

package body Bunches is

   ----------------
   -- Build_List --
   ----------------

   procedure Build_List is
   begin
      HTML.Comment ("Bug #1830: Bunches.Build_List called");
      SGE.Bunches.Build_List;
   end Build_List;


   procedure Put_List is
   begin
      Ada.Text_IO.Put_Line ("<table><tr>");
      HTML.Put_Cell (Data => "<acronym title=""click on arrow to view job list"">"
                     & "Detail</acronym>", Tag => "th");
      HTML.Put_Cell (Data => "<acronym title=""Parallel Environment"">PE</acronym>",
                     Tag => "th");
      HTML.Put_Cell (Data => "Slots", Tag => "th");
      HTML.Put_Cell (Data => "", Tag => "th");
      HTML.Put_Cell (Data => "Queue", Tag => "th");
      HTML.Put_Cell (Data => "Hard Requests", Tag => "th");
      HTML.Put_Cell (Data => "Soft Requests", Tag => "th");
      HTML.Put_Cell (Data => "Total", Tag => "th");
      HTML.Put_Cell (Data => "Waiting", Tag => "th");
      HTML.Put_Cell (Data => "Held", Tag => "th");
      HTML.Put_Cell (Data => "Error", Tag => "th");
      Ada.Text_IO.Put_Line ("</tr>");
      SGE.Bunches.Iterate (Put'Access);
   end Put_List;

   ---------
   -- Put --
   --  Purpose: Output one Bunch of Jobs as a single line in a table.
   --  Parameter Pos : Cursor into a List of Bunches, signifies the Bunch to be output
   ---------

   procedure Put (B : SGE.Bunches.Bunch) is
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
      HTML.Put_Cell (Data => Get_Slot_Numbers (B), Class => "right");
      if Has_Balancer (B) then
         HTML.Put_Img_Cell ("balance");
      else
         HTML.Put_Cell ("");
      end if;
      HTML.Put_Cell (Data => Get_Queue (B));
      HTML.Put_Cell (Data => Get_Hard_Resources (B));
      HTML.Put_Cell (Data => Get_Soft_Resources (B));
      HTML.Put_Cell (Data => Get_Total_Jobs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Waiting_Jobs (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Jobs_On_Hold (B)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Errors (B)'Img, Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put;
end Bunches;
