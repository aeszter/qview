with Ada.Text_IO;
with HTML; use HTML;
with CGI;
with SGE.Partitions; use SGE.Partitions;
with SGE.Utils;
with Ada.Exceptions; use Ada.Exceptions;
with Queues;
with SGE.Resources;
with Utils;

package body Partitions is
   procedure Put_Summary_Item (Item : State);

   List : SGE.Partitions.Summarized_List;
   Total_Cores, Total_Hosts : Integer := 0;
   Total_RAM                : SGE.Resources.Gigs := 0.0;
   Total_GPUs, Offline_GPUs : Integer := 0;
   Used_Slots, Used_Hosts,
   Available_Slots, Available_Hosts,
   Disabled_Slots, Disabled_Hosts,
   Offline_Slots, Offline_Hosts : Integer := 0;
   Total_Reserved, Total_Suspended : Integer := 0;

   procedure Count_Slots (Item : Partition);

   procedure Count_Slots (Item : Partition) is
      use SGE.Resources;
   begin
      Total_Cores := Total_Cores + Get_Total_Slots (Item);
      Total_Hosts := Total_Hosts + Get_Total_Hosts (Item);
      Total_RAM := Total_RAM + Get_Total_Hosts (Item) * Get_Memory (Item);
      Total_GPUs := Total_GPUs + Get_Total_Hosts (Item) * Count_GPUs (Item);
      Offline_GPUs := Offline_GPUs + Get_Offline_Hosts (Item) * Count_GPUs (Item);
      Used_Slots := Used_Slots + Get_Used_Slots (Item);
      Used_Hosts := Used_Hosts + Get_Used_Hosts (Item);
      Available_Slots := Available_Slots + Get_Available_Slots (Item);
      Available_Hosts := Available_Hosts + Get_Available_Hosts (Item);
      Disabled_Slots := Disabled_Slots + Get_Disabled_Slots (Item);
      Disabled_Hosts := Disabled_Hosts + Get_Disabled_Hosts (Item);
      Offline_Slots := Offline_Slots + Get_Offline_Slots (Item);
      Offline_Hosts := Offline_Hosts + Get_Offline_Hosts (Item);
      Total_Reserved := Total_Reserved + Get_Reserved_Hosts (Item);
      Total_Suspended := Total_Suspended + Get_Suspended_Slots (Item);
   end Count_Slots;

   procedure Put_List is
      use SGE.Resources;
   begin
      Ada.Text_IO.Put_Line ("<table>");
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => "<acronym title=""click on arrow to view node list"">"
                  & "Detail</acronym>", Tag => "th");
      HTML.Put_Cell (Data => "Interconnect", Tag => "th");
      HTML.Put_Cell (Data       => "Resources",
                     Tag        => "th");
      HTML.Put_Cell (Data => "GPU",
                     Tag  => "th");
      HTML.Put_Cell (Data => "Memory",
                     Tag  => "th");
      HTML.Put_Cell (Data => "CPU" & HTML.Help_Icon (Topic => "CPU Families"),
                  Tag => "th");
      HTML.Put_Cell (Data => "Cores", Tag => "th");
      HTML.Put_Cell (Data => "RAM", Tag => "th");
      HTML.Put_Cell (Data => "Runtime", Tag => "th");
      HTML.Put_Cell (Data => "Slots", Tag => "th");
      HTML.Put_Cell (Data => "Hosts", Tag => "th");
      HTML.Put_Cell (Data => "Used", Tag => "th");
      HTML.Put_Cell (Data => "Reserved", Tag => "th");
      HTML.Put_Cell (Data => "Available", Tag => "th");
      HTML.Put_Cell (Data => "<acronym title=""d: disabled by admin or health checker; D: disabled by calendar"">Disabled</acronym>", Tag => "th");
      HTML.Put_Cell ("<acronym title=""u: unreacheable"">Offline</acronym>", Tag => "th");
      HTML.Put_Cell ("<acronym title=""S: suspended by a competing queue"">Suspended</acronym>", Tag => "th");
      Ada.Text_IO.Put_Line ("</tr>");
      SGE.Partitions.Iterate (List, Put'Access);
      SGE.Partitions.Iterate (List, Count_Slots'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell (Data    => "",
                     Colspan => 2);
      HTML.Put_Cell (Data    => "Totals:",
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Total_GPUs) &
                       " GPUs (" & Integer'Image (Offline_GPUs) & " offline)",
                     Class   => "right");
      HTML.Put_Cell (Data    => "",
                     Colspan => 3);
      HTML.Put_Cell (Data    => To_String (Total_RAM / 1_024) & "T",
                     Class   => "right");
      HTML.Put_Cell (Data    => "");
      HTML.Put_Cell (Data    => Integer'Image (Total_Cores),
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Total_Hosts),
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Used_Slots) &
                       " (" & Integer'Image (Used_Hosts) & ")",
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Total_Reserved),
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Available_Slots) &
                       " (" & Integer'Image (Available_Hosts) & ")",
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Disabled_Slots) &
                       " (" & Integer'Image (Disabled_Hosts) & ")",
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Offline_Slots) &
                       " (" & Integer'Image (Offline_Hosts) & ")",
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Total_Suspended),
                     Class   => "right");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell (Data    => "",
                     Colspan => 5);
      HTML.Put_Cell (Data    => "Million Core-hours per Year:",
                     Colspan => 4,
                     Class   => "right");
      HTML.Put_Cell (Integer'Image (Total_Cores * 24 * 356 / 1_000_000),
                     Class   => "right");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
   end Put_List;

   procedure Put (P : Partition) is
      package Str renames Ada.Strings;
      use SGE.Utils.String_Lists;
      use SGE.Resources;

      procedure Put_Error (Message : String);

      procedure Put_Error (Message : String) is
      begin
         HTML.Comment (Message);
      end Put_Error;

      GPU_Present : Boolean;
      Config_Error : Boolean := False;
      GPU_Memory   : constant String := P.Get_GPU_Memory;
   begin
      begin
         GPU_Present := Has_GPU (P);
      exception
         when SGE.Utils.Operator_Error =>
            Config_Error := True;
            GPU_Present := False;
      end;
      if Get_Available_Hosts (P) > 0 then
         Ada.Text_IO.Put ("<tr class=""available"">");
      elsif Get_Available_Slots (P) > 0 then
         Ada.Text_IO.Put ("<tr class=""slots_available"">");
      elsif Get_Offline_Slots (P) = Get_Total_Slots (P) then
         Ada.Text_IO.Put ("<tr class=""offline"">");
      elsif P.Has_Errors or else Config_Error then
         Ada.Text_IO.Put ("<tr class=""program_error"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;
      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?hosts=partition"
                     & "&net=" & Get_Network (P)
                     & "&gm=" & To_String (Get_GPU (P))
                     & "&model=" & To_String (Get_Model (P))
                     & "&cores=" & Get_Cores (P)'Img
                     & "&mem=" & Get_Memory (P)
                     & "&slots=" & P.Get_Slots'Img
                     & "&gpu=" & GPU_Present'Img
                     & "&ssd=" & Has_SSD (P)'Img
                     & """><img src=""/icons/arrow_right.png"" /></a>");

      HTML.Put_Cell (Data => Get_Network (P));
      Ada.Text_IO.Put ("<td>");
      if GPU_Present then
         Ada.Text_IO.Put (HTML.Img_Tag ("GPU"));
      end if;
      if Has_SSD (P) then
         Ada.Text_IO.Put (HTML.Img_Tag ("SSD"));
      end if;
      Ada.Text_IO.Put ("</td>");
      HTML.Put_Cell (Data => To_String (Get_GPU (P)));
      if GPU_Memory /= "" then
         HTML.Put_Cell (Data => GPU_Memory & "G", Class => "right");
      else
         HTML.Put_Cell ("");
      end if;
      HTML.Put_Cell (Data => To_String (Get_Model (P)));
      if Get_Cores (P) = Get_Slots (P) then
         HTML.Put_Cell (Data => Get_Cores (P)'Img, Class => "right");
      else
         HTML.Put_Cell (Data  => Get_Slots (P)'Img & "/" &
                          Utils.To_String (Get_Cores (P)),
                        Class => "right");
      end if;
      HTML.Put_Cell (Data => Get_Memory (P) & "G", Class => "right");
      HTML.Put_Cell (Data => Get_Runtime (P), Class => "right");
      HTML.Put_Cell (Data => P.Get_Total_Slots'Img, Class => "right");
      HTML.Put_Cell (Data => P.Get_Total_Hosts'Img, Class => "right");
      HTML.Put_Cell (Data => P.Get_Used_Slots'Img & " ("
                     & Utils.To_String (P.Get_Used_Hosts)
                     & ")", Class => "right");
      HTML.Put_Cell (Data => P.Get_Reserved_Slots'Img, Class => "right");
      HTML.Put_Cell (Data  => P.Get_Available_Slots'Img & " ("
                     & Utils.To_String (P.Get_Available_Hosts) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => P.Get_Disabled_Slots'Img
                     & " (" & Utils.To_String (P.Get_Disabled_Hosts) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => P.Get_Offline_Slots'Img
                     & " (" & Utils.To_String (P.Get_Offline_Hosts) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => P.Get_Suspended_Slots'Img,
                     Class => "right");
      if Config_Error then
         HTML.Put_Cell ("inconsistent GPU config encountered");
      end if;
      Ada.Text_IO.Put ("</tr>");
      if Has_Errors (P) then
         P.Iterate_Errors (Put_Error'Access);
      end if;
   exception
      when E : SGE.Utils.Operator_Error =>
         HTML.Put_Paragraph (Label    => "Misconfiguration",
                             Contents => Exception_Message (E));
         Ada.Text_IO.Put ("</tr>"); -- close open row
   end Put;

   procedure Put_Summary_Item (Item : State) is
   begin
      Ada.Text_IO.Put ("<li>");
      Ada.Text_IO.Put (Get_Summary (List, Item)'Img & " ");
      Ada.Text_IO.Put (To_String (Item));
      Ada.Text_IO.Put ("</li>");
   end Put_Summary_Item;

   procedure Put_Summary is
   begin
      HTML.Begin_Div (ID => "partition_summary");
      Ada.Text_IO.Put ("<ul>");
      Iterate_Summary (Put_Summary_Item'Access);
      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "partition_summary");
   end Put_Summary;

   procedure Build_List is
   begin
      Queues.Partition (List);
   end Build_List;

end Partitions;
