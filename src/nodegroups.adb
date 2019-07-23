with Ada.Text_IO;
with Ada.Strings.Fixed;

with Slurm.Utils; use Slurm.Utils;
with Slurm.Nodegroups; use Slurm.Nodegroups;

with HTML; use HTML;
with CGI;

package body Nodegroups is

   procedure Put_Summary_Item (Item : State);

   All_Groups : Slurm.Nodegroups.Summarized_List;
   Total_Cores, Total_Nodes : Integer := 0;
   Used_Cores, Used_Nodes,
   Available_Cores, Available_Nodes,
   Draining_Cores, Draining_Nodes,
   Offline_Cores, Offline_Nodes : Integer := 0;

   procedure Count_Slots (Item : Nodegroup);

   procedure Count_Slots (Item : Nodegroup) is
   begin
      Total_Cores := Total_Cores + Get_Total_Cores (Item);
      Total_Nodes := Total_Nodes + Get_Total_Nodes (Item);
      Used_Cores := Used_Cores + Get_Used_Cores (Item);
      Used_Nodes := Used_Nodes + Get_Used_Nodes (Item);
      Available_Cores := Available_Cores + Get_Available_Cores (Item);
      Available_Nodes := Available_Nodes + Get_Available_Nodes (Item);
      Draining_Cores := Draining_Cores + Get_Drained_Cores (Item);
      Draining_Nodes := Draining_Nodes + Get_Drained_Nodes (Item);
      Offline_Cores := Offline_Cores + Get_Offline_Cores (Item);
      Offline_Nodes := Offline_Nodes + Get_Offline_Nodes (Item);
   end Count_Slots;

   procedure Put (G : Nodegroup) is
      package Str renames Ada.Strings;
      package Str_F renames Str.Fixed;

      procedure Put_Error (Message : String);
      procedure Put_Error (Message : String) is
      begin
         HTML.Comment (Message);
      end Put_Error;

   begin
      if G.Has_Errors then
         Ada.Text_IO.Put ("<tr class=""program_error"">");
      elsif Get_Available_Nodes (G) > 0 then
         Ada.Text_IO.Put ("<tr class=""available"">");
      elsif Get_Available_Cores (G) > 0 then
         Ada.Text_IO.Put ("<tr class=""slots_available"">");
      elsif Get_Offline_Cores (G) = Get_Total_Cores (G) then
         Ada.Text_IO.Put ("<tr class=""offline"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;
      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?nodes=group"
                     & "&gres=" & HTML.To_Web_String (Get_GRES (G))
                     & "&tres=" & HTML.To_Web_String (Get_TRES (G))
                     & "&features=" & Get_Features (G)
                     & "&cores=" & Get_CPUs (G)'Img
                     & "&mem=" & To_String (Get_Memory (G))
                     & """><img src=""/icons/arrow_right.png"" /></a>");

      HTML.Put_Cell (Data => Get_Features (G));
      HTML.Put_Cell (Data => HTML.To_String (Get_GRES (G), 1));
      HTML.Put_Cell (Data => Get_CPUs (G)'Img, Class => "right");
      HTML.Put_Cell (Data => To_String (Get_Memory (G)) & "G", Class => "right");
      HTML.Put_Cell (Data => Get_Total_Cores (G)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Total_Nodes (G)'Img, Class => "right");
      HTML.Put_Cell (Data => Get_Used_Cores (G)'Img & " ("
                     & Str_F.Trim (Get_Used_Nodes (G)'Img, Str.Left)
                     & ")", Class => "right");
      HTML.Put_Cell (Data  => Get_Available_Cores (G)'Img & " ("
                     & Str_F.Trim (Get_Available_Nodes (G)'Img, Str.Left) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => Get_Drained_Cores (G)'Img
                     & " (" & Str_F.Trim (Get_Drained_Nodes (G)'Img, Str.Left) & ")",
                     Class => "right");
      HTML.Put_Cell (Data  => Get_Offline_Cores (G)'Img
                     & " (" & Str_F.Trim (Get_Offline_Nodes (G)'Img, Str.Left) & ")",
                     Class => "right");
      Ada.Text_IO.Put ("</tr>");
      if G.Has_Errors then
         G.Iterate_Errors (Put_Error'Access);
      end if;
   end Put;

   procedure Put_All is
   begin
      All_Groups := Slurm.Nodegroups.Load;
      Put_Summary;
      Put_List (All_Groups);
   end Put_All;

   procedure Put_List (Source : Slurm.Nodegroups.Summarized_List) is
      package Str renames Ada.Strings;
      package Str_F renames Str.Fixed;
   begin
      Ada.Text_IO.Put_Line ("<table>");
      Ada.Text_IO.Put ("<tr>");
      HTML.Put_Cell (Data => "<acronym title=""click on arrow to view node list"">"
                  & "Detail</acronym>", Tag => "th");
      HTML.Put_Cell (Data => "Features", Tag => "th");
      HTML.Put_Cell (Data       => "Resources",
                     Tag        => "th");
      HTML.Put_Cell (Data => "Cores", Tag => "th");
      HTML.Put_Cell (Data => "RAM", Tag => "th");
      HTML.Put_Cell (Data => "Slots", Tag => "th");
      HTML.Put_Cell (Data => "Nodes", Tag => "th");
      HTML.Put_Cell (Data => "Used", Tag => "th");
      HTML.Put_Cell (Data => "Available", Tag => "th");
      HTML.Put_Cell (Data => "Draining",
                     Tag  => "th");
      HTML.Put_Cell ("Offline", Tag => "th");
      Ada.Text_IO.Put_Line ("</tr>");
      Slurm.Nodegroups.Iterate (Source, Put'Access);
      Slurm.Nodegroups.Iterate (Source, Count_Slots'Access);
      Ada.Text_IO.Put_Line ("<tr>");
      HTML.Put_Cell (Data    => "",
                     Colspan => 4);
      HTML.Put_Cell (Data    => "Totals:",
                     Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Total_Cores),
                       Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Total_Nodes),
                       Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Used_Cores) & " (" &
                                Str_F.Trim (Integer'Image (Used_Nodes), Str.Left) & ")",
                       Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Available_Cores) & " (" &
                                Str_F.Trim (Integer'Image (Available_Nodes), Str.Left) & ")",
                       Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Draining_Cores) & " (" &
                                Str_F.Trim (Integer'Image (Draining_Nodes), Str.Left) & ")",
                       Class   => "right");
      HTML.Put_Cell (Data    => Integer'Image (Offline_Cores) & " (" &
                                Str_F.Trim (Integer'Image (Offline_Nodes), Str.Left) & ")",
                       Class   => "right");
      Ada.Text_IO.Put_Line ("</tr><tr>");
      HTML.Put_Cell (Data    => "");
      HTML.Put_Cell (Data    => "Million Core-hours per Year:",
                     Colspan => 4,
                     Class   => "right");
      HTML.Put_Cell (Integer'Image (Total_Cores * 24 * 356 / 1_000_000),
        Class   => "right");
      Ada.Text_IO.Put_Line ("</tr>");
      Ada.Text_IO.Put_Line ("</table>");
   end Put_List;

   procedure Put_Summary  is
   begin
      HTML.Begin_Div (ID => "nodegroup_summary");
      Ada.Text_IO.Put ("<ul>");
      Iterate_Summary (Put_Summary_Item'Access);
      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "nodegroup_summary");
   end Put_Summary;

   procedure Put_Summary_Item (Item : State) is
   begin
      Ada.Text_IO.Put ("<li>");
      Ada.Text_IO.Put (Get_Summary (All_Groups, Item)'Img & " ");
      Ada.Text_IO.Put (To_String (Item));
      Ada.Text_IO.Put ("</li>");
   end Put_Summary_Item;

end Nodegroups;
