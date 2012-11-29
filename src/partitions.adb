with Ada.Containers.Doubly_Linked_Lists;
with Queues;
with Ada.Text_IO;
with HTML; use HTML;
with Resources; use Resources;
with CGI;
with Ada.Strings.Fixed;


package body Partitions is

   ---------
   -- "=" --
   ---------

   function "=" (Left : Partition; Right : Queue) return Boolean is
   begin
      return Left.Properties = Get_Properties (Right);
   end "=";

   function "=" (Left : Queue; Right : Partition) return Boolean is
   begin
      return Right = Left;
   end "=";


   -------------------
   -- New_Partition --
   --  Purpose: Build a partition list from a queue list.
   --           This totals all slots (available, used, reserved, ...) for the
   --           for all matching queues.
   --  Parameter Q_List: List of queues to work on.
   --  Parameter Part_List: The new partition list.
   --  Side Effect: Q_List is sorted by resources.
   -------------------

   procedure Build_List is
      P : Partition;
      Q : Queue;
   begin
      Queues.Sort;
      Queues.Rewind;

      Q := Queues.Current;
      --  Create Partition according to first Queue
      P := New_Partition (Q);
      loop
         --  New Partition?
         if P /= Q then
            --  Yes. Store previous one.
            List.Append (P);
            P := New_Partition (Q);
         end if;

         --  Update totals
         P.Total_Slots := P.Total_Slots + Get_Slot_Count (Q);
         P.Total_Hosts := P.Total_Hosts + 1;
         List.Summary (total) := List.Summary (total) + Get_Slot_Count (Q);
         if Is_Offline (Q) then
            P.Offline_Slots := P.Offline_Slots + Get_Slot_Count (Q);
            P.Offline_Hosts := P.Offline_Hosts + 1;
            List.Summary (offline) := List.Summary (offline) + Get_Slot_Count (Q);
         elsif Is_Suspended (Q) then
            P.Suspended_Slots := P.Suspended_Slots + Get_Slot_Count (Q);
            P.Suspended_Hosts := P.Suspended_Hosts + 1;
            List.Summary (suspended) := List.Summary (suspended) + Get_Slot_Count (Q);
         else
            if Get_Used_Slots (Q) > 0 then
               P.Used_Hosts := P.Used_Hosts + 1;
               P.Used_Slots := P.Used_Slots + Get_Used_Slots (Q);
               List.Summary (used) := List.Summary (used) + Get_Used_Slots (Q);
            end if;
            if Get_Reserved_Slots (Q) > 0 then
               P.Reserved_Hosts := P.Reserved_Hosts + 1;
               P.Reserved_Slots := P.Reserved_Slots + Get_Reserved_Slots (Q);
               List.Summary (reserved) := List.Summary (reserved) + Get_Reserved_Slots (Q);
            end if;
            P.Available_Slots := P.Available_Slots + Get_Free_Slots (Q);
            if Get_Reserved_Slots (Q) = 0 and then Get_Used_Slots (Q) = 0 then
               P.Available_Hosts := P.Available_Hosts + 1;
            end if;
            List.Summary (available) := List.Summary (available) + Get_Free_Slots (Q);
         end if;
         exit when Queues.At_End;
         --  Advance
         Q := Queues.Next;
      end loop;
      --  That's it. Store final partition.
      List.Append (P);
   end Build_List;

   -------------------
   -- New_Partition --
   -------------------

   function New_Partition (Q : Queue) return Partition is
      P : Partition;
   begin
      P.Properties := Get_Properties (Q);
      P.Name      := Get_Name (Q);
      return P;
   end New_Partition;


   procedure Put_List is
   begin
      List.Iterate (Put'Access);
   end Put_List;


   procedure Put (Partition : Partitions.Partition_Lists.Cursor) is
      package Str renames Ada.Strings;
      package Str_F renames Str.Fixed;
      P : Partitions.Partition := Partitions.Partition_Lists.Element (Partition);
      Props : Set_Of_Properties := P.Properties;
   begin
      if P.Available_Hosts > 0 then
         Ada.Text_IO.Put ("<tr class=""available"">");
      elsif P.Available_Slots > 0 then
         Ada.Text_IO.Put ("<tr class=""slots_available"">");
      elsif P.Offline_Slots = P.Total_Slots then
         Ada.Text_IO.Put ("<tr class=""offline"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;
      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?hosts=partition"
                     & "&net=" & Get_Network (Props)'Img
                     & "&model=" & Get_Model (Props)'Img
                     & "&cores=" & Get_Cores (Props)'Img
                     & "&mem=" & To_String (Get_Memory (Props))
                     & "&q=" & To_String (P.Name)
                     & "&gpu=" & Has_GPU (Props)'Img
                     & "&ssd=" & Has_SSD (Props)'Img
                     & """><img src=""/icons/arrow_right.png"" /></a>");

      HTML.Put_Cell (Data => Get_Network (Props)'Img);
      Ada.Text_IO.Put ("<td>");
      if Has_GPU (Props) then
         Ada.Text_IO.Put (HTML.Img_Tag ("GPU"));
      end if;
      if Has_SSD (Props) then
         Ada.Text_IO.Put (HTML.Img_Tag ("SSD"));
      end if;
      Ada.Text_IO.Put ("</td>");
      HTML.Put_Cell (Data => Get_Model (Props)'Img);
      HTML.Put_Cell (Data => Get_Cores (Props)'Img, Class => "right");
      HTML.Put_Cell (Data => To_String (Get_Memory (Props)) & "G", Class => "right");
      HTML.Put_Cell (Data => Get_Runtime (Props), Class => "right");
      HTML.Put_Cell (Data => P.Total_Slots'Img, Class => "right");
      HTML.Put_Cell (Data => P.Total_Hosts'Img, Class => "right");
      HTML.Put_Cell (Data => P.Used_Slots'Img & " (" & Str_F.Trim (P.Used_Hosts'Img, Str.Left) & ")", Class => "right");
      HTML.Put_Cell (Data => P.Reserved_Slots'Img, Class => "right");
      HTML.Put_Cell (Data => P.Available_Slots'Img & " (" & Str_F.Trim (P.Available_Hosts'Img, Str.Left) & ")", Class => "right");
      HTML.Put_Cell (Data => P.Suspended_Slots'Img & " (" & Str_F.Trim (P.Suspended_Hosts'Img, Str.Left) & ")", Class => "right");
      HTML.Put_Cell (Data => P.Offline_Slots'Img & " (" & Str_F.Trim (P.Offline_Hosts'Img, Str.Left) & ")", Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put;

   procedure Put_Summary is
   begin
      HTML.Begin_Div (ID => "partition_summary");
      Ada.Text_IO.Put ("<ul>");
      for State in List.Summary'Range loop
         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put (List.Summary (State)'Img & " ");
         Ada.Text_IO.Put (To_String (State));
         Ada.Text_IO.Put ("</li>");
      end loop;
      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "partition_summary");
   end Put_Summary;

   function To_String (Source : State) return String is
   begin
      case Source is
         when total =>
            return "Total";
         when reserved =>
            return "Reserved";
         when used =>
            return "Used";
         when offline =>
            return "Offline";
         when available =>
            return "Available";
         when suspended =>
            return "Suspended";
      end case;
   end To_String;

end Partitions;
