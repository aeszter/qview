with Ada.Containers.Doubly_Linked_Lists;
with Queues;
with Resources; use Resources;
with Ada.Text_IO;
with HTML; use HTML;
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
      while not Queues.At_End loop
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

   ---------------------
   -- Model_As_String --
   ---------------------

   function Get_Model (P : Partition) return String is
   begin
      return Get_Model (P.Properties)'Img;
   end Get_Model;

   procedure Put_List is
   begin
      List.Iterate (Put'Access);
   end Put_List;


   procedure Put (Partition : Partitions.Partition_Lists.Cursor) is
      P : Partitions.Partition := Partitions.Partition_Lists.Element (Partition);
   begin
      if P.Available > 0 then
         Ada.Text_IO.Put ("<tr class=""available"">");
      elsif P.Offline = P.Total then
         Ada.Text_IO.Put ("<tr class=""offline"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;
      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?hosts=partition"
                     & "&net=" & P.Network'Img
                     & "&model=" & Model_As_String (P)
                     & "&cores=" & Ada.Strings.Fixed.Trim (P.Cores'Img, Ada.Strings.Left)
                     & "&mem=" & Ada.Strings.Fixed.Trim (P.Memory'Img, Ada.Strings.Left)
                     & "&q=" & To_String (P.Name)
                     & """><img src=""/icons/arrow_right.png"" /></a>");

      HTML.Put_Cell (Data => P.Network'Img);
      HTML.Put_Cell (Data => Model_As_String (P));
      HTML.Put_Cell (Data => P.Cores'Img, Class => "right");
      HTML.Put_Cell (Data => P.Memory'Img & "G", Class => "right");
      HTML.Put_Cell (Data => P.Runtime, Class => "right");
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
         HTML.Put (What => State);
         Ada.Text_IO.Put ("</li>");
      end loop;
      Ada.Text_IO.Put ("</ul>");
      HTML.End_Div (ID => "partition_summary");
   end Put_Summary;


end Partitions;
