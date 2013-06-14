with Ada.Containers.Doubly_Linked_Lists;
with Queues;
with Ada.Text_IO;
with HTML; use HTML;
with Resources; use Resources;
with CGI;
with Ada.Strings.Fixed;


package body Partitions is

   use Queues;
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


   function Sum (Over : Countable_Maps.Map) return Natural is
      Total : Natural := 0;

      procedure Count (Position : Countable_Maps.Cursor) is
      begin
         Total := Total + Countable_Maps.Element (Position);
      end Count;

   begin
      Over.Iterate (Count'Access);
      return Total;
   end Sum;

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

         begin
            --  Update totals
            P.Total_Slots.Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                   New_Item => Get_Slot_Count (Q));
            pragma Compile_Time_Warning (True, "The last queue found for a host will determine the number of slots");
            pragma Compile_Time_Warning (True, "Perhaps look for the maximum instead?");
            P.Total_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
            List.Summary (total).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                   New_Item => Get_Slot_Count (Q));
            if Is_Offline (Q) then
               P.Offline_Slots.Include (Key => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                   New_Item => Get_Slot_Count (Q));
               P.Offline_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
               List.Summary (offline).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Slot_Count (Q));
            elsif Is_Disabled (Q) then
               pragma Compile_Time_Warning (True, "either amend Is_Suspended, or add a new state for (S)uspended queues");
               P.Disabled_Slots.Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Slot_Count (Q));
               P.Disabled_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
               List.Summary (disabled).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                                 New_Item => Get_Slot_Count (Q));
            else
               if Get_Used_Slots (Q) > 0 then
                  P.Used_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
                  P.Used_Slots := P.Used_Slots + Get_Used_Slots (Q);
                  List.Summary (used).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Used_Slots (Q));
               end if;
               if Get_Reserved_Slots (Q) > 0 then
                  P.Reserved_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
                  P.Reserved_Slots := P.Reserved_Slots + Get_Reserved_Slots (Q);
                  List.Summary (reserved).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                                   New_Item => Get_Reserved_Slots (Q));
               end if;
               P.Available_Slots.Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Free_Slots (Q));
               if Get_Reserved_Slots (Q) = 0 and then Get_Used_Slots (Q) = 0 then
                  P.Available_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
               end if;
               List.Summary (available).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                                 New_Item => Get_Free_Slots (Q));
            end if;
         exception
            when Constraint_Error =>
               HTML.Put_Paragraph (Label    => "Warning",
                                   Contents => String'(Get_Long_Name (Q)) & " inconsistent");
         end;
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
      use type Ada.Containers.Count_Type;

      package Str renames Ada.Strings;
      package Str_F renames Str.Fixed;
      P : Partitions.Partition := Partitions.Partition_Lists.Element (Partition);
      Props : Set_Of_Properties := P.Properties;
   begin
      if P.Available_Hosts.Length > 0 then
         Ada.Text_IO.Put ("<tr class=""available"">");
      elsif Sum (P.Available_Slots) > 0 then
         Ada.Text_IO.Put ("<tr class=""slots_available"">");
      elsif Sum (P.Offline_Slots) = Sum (P.Total_Slots) then
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
      Ada.Text_IO.Put ("</tr>");
   end Put;

   procedure Put_Summary is
   begin
      HTML.Begin_Div (ID => "partition_summary");
      Ada.Text_IO.Put ("<ul>");
      for State in List.Summary'Range loop
         Ada.Text_IO.Put ("<li>");
         Ada.Text_IO.Put (Sum (List.Summary (State))'Img & " ");
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
         when disabled =>
            return "Disabled";
      end case;
   end To_String;

end Partitions;
