with Ada.Containers.Doubly_Linked_Lists;
with Queues;
with Ada.Text_IO;
with HTML; use HTML;
with Resources; use Resources;
with CGI;

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
         P.Total := P.Total + Get_Slot_Count (Q);
         List.Summary (total) := List.Summary (total) + Get_Slot_Count (Q);
         if Is_Offline (Q) then
            P.Offline := P.Offline + Get_Slot_Count (Q);
            List.Summary (offline) := List.Summary (offline) + Get_Slot_Count (Q);
         elsif Is_Suspended (Q) then
            P.Suspended := P.Suspended + Get_Slot_Count (Q);
            List.Summary (suspended) := List.Summary (suspended) + Get_Slot_Count (Q);
         else
            P.Used := P.Used + Get_Used_Slots (Q);
            List.Summary (used) := List.Summary (used) + Get_Used_Slots (Q);
            P.Reserved := P.Reserved + Get_Reserved_Slots (Q);
            List.Summary (reserved) := List.Summary (reserved) + Get_Reserved_Slots (Q);
            P.Available := P.Available + Get_Free_Slots (Q);
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
      P.Total     := 0;
      P.Offline   := 0;
      P.Suspended := 0;
      P.Used      := 0;
      P.Reserved  := 0;
      P.Available := 0;
      P.Name      := Get_Name (Q);
      return P;
   end New_Partition;


   procedure Put_List is
   begin
      List.Iterate (Put'Access);
   end Put_List;


   procedure Put (Partition : Partitions.Partition_Lists.Cursor) is
      P : Partitions.Partition := Partitions.Partition_Lists.Element (Partition);
      Props : Set_Of_Properties := P.Properties;
   begin
      if P.Available > 0 then
         Ada.Text_IO.Put ("<tr class=""available"">");
      elsif P.Offline = P.Total then
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
      HTML.Put_Cell (Data => P.Total'Img, Class => "right");
      HTML.Put_Cell (Data => P.Used'Img, Class => "right");
      HTML.Put_Cell (Data => P.Reserved'Img, Class => "right");
      HTML.Put_Cell (Data => P.Available'Img, Class => "right");
      HTML.Put_Cell (Data => P.Suspended'Img, Class => "right");
      HTML.Put_Cell (Data => P.Offline'Img, Class => "right");
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
