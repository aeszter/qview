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


   function Sum (Over : Countable_Map) return Natural is
      Total : Natural := 0;

      procedure Count (Position : Countable_Maps.Cursor) is
      begin
         Total := Total + Countable_Maps.Element (Position);
      end Count;

   begin
      Over.Iterate (Count'Access);
      return Total;
   end Sum;

   overriding procedure Include (Container : in out Countable_Map;
                                 Key       : Host_Name;
                                 New_Item  : Natural) is
      use Countable_Maps;
      Previous : Cursor := Find (Container => Container,
                                 Key       => Key);
      procedure Take_Maximum (Key : Host_Name; Element : in out Natural) is
         pragma Unreferenced (Key);
      begin
         if New_Item > Element then
            Element := New_Item;
         end if;
      end Take_Maximum;

   begin
      if Previous = No_Element then
         Insert (Container => Container,
                 Key       => Key,
                 New_Item  => New_Item);
      else
         Update_Element (Container => Container,
                         Position  => Previous,
                         Process   => Take_Maximum'Access);
      end if;
   end Include;

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
               P.Disabled_Slots.Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                               New_Item => Get_Slot_Count (Q));
               P.Disabled_Hosts.Include (Host_Names.To_Bounded_String (Get_Host_Name (Q)));
               List.Summary (disabled).Include (Key      => Host_Names.To_Bounded_String (Get_Host_Name (Q)),
                                                New_Item => Get_Slot_Count (Q));
            elsif Is_Suspended (Q) then
               P.Suspended_Slots := P.Suspended_Slots + Get_Slot_Count (Q);
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
      List.Iterate (Put'Access);
      Ada.Text_IO.Put_Line ("</table>");
      HTML.End_Div (Class => "partitions");
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
      HTML.Put_Cell (Data  => P.Suspended_Slots'Img,
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
