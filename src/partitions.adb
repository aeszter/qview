with Ada.Text_IO;
with HTML; use HTML;
with CGI;
with Ada.Strings.Fixed;
with SGE.Partitions; use SGE.Partitions;
with SGE.Utils;

package body Partitions is


   procedure Put_List is
   begin
      Ada.Text_IO.Put_Line ("<table>");
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => "<acronym title=""click on arrow to view node list"">"
                  & "Detail</acronym>", Tag => "th");
      HTML.Put_Cell (Data => "Interconnect", Tag => "th");
      HTML.Put_Cell (Data       => "Resources",
                     Tag        => "th");
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
      HTML.Put_Cell (Data => "<acronym title=""d: disabled by admin or health checker"">Disabled</acronym>", Tag => "th");
      HTML.Put_Cell ("<acronym title=""u: unreacheable"">Offline</acronym>", Tag => "th");
      HTML.Put_Cell ("<acronym title=""S: suspended by a competing queue"">Suspended</acronym>", Tag => "th");
      Ada.Text_IO.Put_Line ("</tr>");
      SGE.Partitions.Iterate (Put'Access);
      Ada.Text_IO.Put_Line ("</table>");
   end Put_List;


   procedure Put (P : Partition) is
      package Str renames Ada.Strings;
      package Str_F renames Str.Fixed;

      use SGE.Utils.String_Lists;

      procedure Put_Error (Position : SGE.Utils.String_Lists.Cursor) is
      begin
         HTML.Comment (Element (Position));
      end Put_Error;

   begin
      if Get_Available_Hosts (P) > 0 then
         Ada.Text_IO.Put ("<tr class=""available"">");
      elsif Get_Available_Slots (P) > 0 then
         Ada.Text_IO.Put ("<tr class=""slots_available"">");
      elsif Get_Offline_Slots (P) = Get_Total_Slots (P) then
         Ada.Text_IO.Put ("<tr class=""offline"">");
      elsif not P.Error_Log.Is_Empty then
         Ada.Text_IO.Put ("<tr class=""program_error"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;
      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?hosts=partition"
                     & "&net=" & Get_Network (P)
                     & "&model=" & Get_Model (P)
                     & "&cores=" & Get_Cores (P)'Img
                     & "&mem=" & Get_Memory (P)
                     & "&q=" & To_String (P.Name)
                     & "&gpu=" & Has_GPU (P)'Img
                     & "&ssd=" & Has_SSD (P)'Img
                     & """><img src=""/icons/arrow_right.png"" /></a>");

      HTML.Put_Cell (Data => Get_Network (P));
      Ada.Text_IO.Put ("<td>");
      if Has_GPU (P) then
         Ada.Text_IO.Put (HTML.Img_Tag ("GPU"));
      end if;
      if Has_SSD (P) then
         Ada.Text_IO.Put (HTML.Img_Tag ("SSD"));
      end if;
      Ada.Text_IO.Put ("</td>");
      HTML.Put_Cell (Data => Get_Model (P));
      HTML.Put_Cell (Data => Get_Cores (P)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Memory (P) & "G", Class => "right");
      HTML.Put_Cell (Data => Get_Runtime (P), Class => "right");
      HTML.Put_Cell (Data => Sum (P.Total_Slots)'Img, Class => "right");
      HTML.Put_Cell (Data => P.Total_Hosts.Length'Img, Class => "right");
      HTML.Put_Cell (Data => P.Used_Slots'Img & " ("
                     & Str_F.Trim (P.Used_Hosts.Length'Img, Str.Left)
                     & ")", Class => "right");
      HTML.Put_Cell (Data => P.Reserved_Slots'Img, Class => "right");
      HTML.Put_Cell (Data  => Sum (P.Available_Slots)'Img & " ("
                     & Str_F.Trim (P.Available_Hosts.Length'Img, Str.Left) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => Sum (P.Disabled_Slots)'Img
                     & " (" & Str_F.Trim (P.Disabled_Hosts.Length'Img, Str.Left) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => Sum (P.Offline_Slots)'Img
                     & " (" & Str_F.Trim (P.Offline_Hosts.Length'Img, Str.Left) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => P.Suspended_Slots'Img,
                     Class => "right");
      Ada.Text_IO.Put ("</tr>");
      if not P.Error_Log.Is_Empty then
         P.Error_Log.Iterate (Put_Error'Access);
      end if;
   end Put;

   procedure Put_Summary_Item (Item : State) is
   begin
      Ada.Text_IO.Put ("<li>");
      Ada.Text_IO.Put (Get_Summary (Item)'Img & " ");
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
      HTML.Bug_Ref (Bug_ID => 1830, Info => "Partitions.Build_List called");
      SGE.Partitions.Build_List;
   end Build_List;

end Partitions;
