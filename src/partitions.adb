with Ada.Containers.Doubly_Linked_Lists;
with Queues; use Queues.Queue_Lists;
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
      return Left.Network = Right.Network and then
             Left.Model = Right.Model and then
             Left.Memory = Right.Memory and then
             Left.Cores = Right.Cores and then
             Left.Runtime = Right.Runtime;
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

   procedure Build_List (Q_List : in out Queues.Queue_Lists.List;
                          Part_List : out Partition_Lists.List) is
      P : Partition;
      Q : Queue;
      Cursor : Queues.Queue_Lists.Cursor;
   begin
      Sorting_By_Resources.Sort (Q_List);
      Cursor := Q_List.First;
      Q := Element (Cursor);

      --  Create Partition according to first Queue
      P := New_Partition (Q);
      while Cursor /= No_Element loop
         Q := Element (Cursor);
         --  New Partition?
         if P /= Q then
            --  Yes. Store previous one.
            Part_List.Append (P);
            P := New_Partition (Q);
         end if;

         --  Update totals
         P.Total := P.Total + Q.Total;
         if Q.Offline then
            P.Offline := P.Offline + Q.Total;
         elsif Q.Suspended then
            P.Suspended := P.Suspended + Q.Total;
         else
            P.Used := P.Used + Q.Used;
            P.Reserved := P.Reserved + Q.Reserved;
            P.Available := P.Available + Q.Total - Q.Reserved - Q.Used;
         end if;
         --  Advance
         Cursor := Next (Cursor);
      end loop;
      --  That's it. Store final partition.
      Part_List.Append (P);
   end Build_List;

   -------------------
   -- New_Partition --
   -------------------

   function New_Partition (Q : Queue) return Partition is
      P : Partition;
   begin
      P.Network   := Q.Network;
      P.Memory    := Q.Memory;
      P.Cores     := Q.Cores;
      P.Runtime   := Q.Runtime;
      P.Model     := Q.Model;
      P.Total     := 0;
      P.Offline   := 0;
      P.Suspended := 0;
      P.Used      := 0;
      P.Reserved  := 0;
      P.Available := 0;
      return P;
   end New_Partition;

   ---------------------
   -- Model_As_String --
   ---------------------

   function Model_As_String (P : Partition) return String is
   begin
      return To_String (P.Model);
   end Model_As_String;

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
                     & "&rt=" & To_String (P.Runtime)
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

end Partitions;
