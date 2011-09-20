with Ada.Containers.Doubly_Linked_Lists;
with Jobs; use Jobs;
with Ranges; use Ranges; use Ranges.Range_Lists;
with Ada.Text_IO;
with CGI;
with HTML;
with Resources; use Resources; use Resources.Resource_Lists;
with Ada.Exceptions;

package body Bunches is

   ----------------
   -- Build_List --
   ----------------

   procedure Build_List is
      B : Bunch;
      J : Job;
   begin
      Jobs.Sort;
      Jobs.Rewind;

      if Jobs.Empty then
         Ada.Text_IO.Put_Line ("<i>No jobs found</i>");
      else
         J := Jobs.Current;
         --  Create Bunch according to first Job
         B := New_Bunch (J);
         while not Jobs.At_End loop
            --  New Bunch?
            if B /= J then
               --  Yes. Store previous one.
               List.Append (B);
               B := New_Bunch (J);
               HTML.Comment ("Store Bunch");
            end if;

            --  Update totals

            B.Total := B.Total + Get_Task_Count (J);
            if On_Hold (J) then
               B.On_Hold := B.On_Hold + Get_Task_Count (J);
            elsif Has_Error (J) then
               B.Error := B.Error + Get_Task_Count (J);
            else
               B.Waiting := B.Waiting + Get_Task_Count (J);
            end if;
            --  Advance
            J := Jobs.Next;
         end loop;
         --  That's it. Store final bunch.
         List.Append (B);
      end if;
   exception
      when E : Constraint_Error
         => HTML.Error ("Unable to build bunch while examining job"
                        & Get_ID (J)
                       & ": " & Ada.Exceptions.Exception_Message (E));
   end Build_List;

   -------------------
   -- New_Bunch --
   -------------------

   function New_Bunch (J : Job) return Bunch is
      B : Bunch;
   begin
      B.PE          := Get_PE (J);
      B.Slot_List   := Get_Slot_List (J);
      B.Slot_Number := Get_Slot_Number (J);
      B.Queue       := Get_Queue (J);
      B.Hard        := Get_Hard_Resources (J);
      B.Soft        := Get_Soft_Resources (J);
      B.Total       := 0;
      B.On_Hold     := 0;
      B.Waiting     := 0;
      B.Error       := 0;
      return B;
   end New_Bunch;


   procedure Put_List is
   begin
      List.Iterate (Put'Access);
   end Put_List;

   ---------
   -- Put --
   --  Purpose: Output one Bunch of Jobs as a single line in a table.
   --  Parameter Pos : Cursor into a List of Bunches, signifies the Bunch to be output
   ---------

   procedure Put (Pos : Bunch_Lists.Cursor) is
      B : Bunch := Bunch_Lists.Element (Pos);
   begin
      if B.Error > 0 then
         Ada.Text_IO.Put ("<tr class=""job-error"">");
      elsif B.Waiting = 0 then
         Ada.Text_IO.Put ("<tr class=""job-held"">");
      else
         Ada.Text_IO.Put ("<tr>");
      end if;

      HTML.Put_Cell (Data => "<a href=""" & CGI.My_URL & "?jobs=bunch"
               & "&pe=" & B.PE
               & "&slot_ranges=" & Ranges.Hash (B.Slot_List)
               & "&slot_number=" & B.Slot_Number
               & "&queue=" & B.Queue
               & "&hr=" & Resources.Hash (B.Hard)
               & "&sr=" & Hash (B.Soft)
               & """><img src=""/icons/arrow_right.png"" /></a>");

      HTML.Put_Cell (Data => B.PE);
      if not B.Slot_List.Is_Empty then
         Ranges.Put_Cell (Data => B.Slot_List, Class => "right");
      else
         HTML.Put_Cell (Data => B.Slot_Number);
      end if;
      HTML.Put_Cell (Data => B.Queue);
      HTML.Put_Cell (Data => To_Unbounded_String (B.Hard));
      HTML.Put_Cell (Data => To_Unbounded_String (B.Soft));
      HTML.Put_Cell (Data => B.Total'Img, Class => "right");
      HTML.Put_Cell (Data => B.Waiting'Img, Class => "right");
      HTML.Put_Cell (Data => B.On_Hold'Img, Class => "right");
      HTML.Put_Cell (Data => B.Error'Img, Class => "right");
      Ada.Text_IO.Put ("</tr>");
   end Put;

   ---------
   -- "=" --
   ---------

   function "=" (Left : Job; Right : Bunch) return Boolean is
   begin
      return Right = Left;
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left : Bunch; Right : Job) return Boolean is
   begin
      return (Left.PE = Get_PE (Right) and then
                   Left.Slot_Number = Get_Slot_Number (Right) and then
                   Left.Slot_List = Get_Slot_List (Right) and then
                   Left.Hard = Get_Hard_Resources (Right) and then
                   Left.Soft = Get_Soft_Resources (Right) and then
                   Left.Queue = Get_Queue (Right));
   end "=";
end Bunches;
