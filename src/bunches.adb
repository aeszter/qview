with Ada.Containers.Doubly_Linked_Lists;
with Jobs; use Jobs; use Jobs.Job_Lists;
with Slots; use Slots; use Slots.Slot_Lists;
with Ada.Text_IO;
with CGI;
with HTML;
with Resources; use Resources; use Resources.Resource_Lists;
with Ada.Exceptions;

package body Bunches is

   ----------------
   -- Build_List --
   ----------------

   procedure Build_List
     (Job_List : in out Jobs.Job_Lists.List;
      Bunch_List : out Bunch_Lists.List)
   is
      B : Bunch;
      J : Job;
      Cursor : Jobs.Job_Lists.Cursor;
   begin
      Sorting_By_Resources.Sort (Job_List);
      Cursor := Job_List.First;

      if Cursor = Jobs.Job_Lists.No_Element then
         Ada.Text_IO.Put_Line ("<i>No jobs found</i>");
      else
         J := Element (Cursor);

         --  Create Bunch according to first Job
         B := New_Bunch (J);
         while Cursor /= Jobs.Job_Lists.No_Element loop
            J := Element (Cursor);
            --  New Bunch?
            if B /= J then
               --  Yes. Store previous one.
               Bunch_List.Append (B);
               B := New_Bunch (J);
            end if;

            --  Update totals
            B.Total := B.Total + 1;
            if On_Hold (J) then
               B.On_Hold := B.On_Hold + 1;
            elsif Has_Error (J) then
               B.Error := B.Error + 1;
            else
               B.Waiting := B.Waiting + 1;
            end if;
            --  Advance
            Cursor := Next (Cursor);
         end loop;
         --  That's it. Store final bunch.
         Bunch_List.Append (B);
      end if;
   exception
      when E : Constraint_Error
         => HTML.Error ("Unable to build bunch while examining job"
                        & J.Number'Img
                       & ": " & Ada.Exceptions.Exception_Message (E));
   end Build_List;

   -------------------
   -- New_Bunch --
   -------------------

   function New_Bunch (J : Job) return Bunch is
      B : Bunch;
   begin
      B.PE          := J.PE;
      B.Slot_List   := J.Slot_List;
      B.Slot_Number := J.Slot_Number;
      B.Queue       := J.Queue;
      B.Hard        := J.Hard;
      B.Soft        := J.Soft;
      B.Total       := 0;
      B.On_Hold     := 0;
      B.Waiting     := 0;
      B.Error       := 0;
      return B;
   end New_Bunch;


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
               & "&slot_ranges=" & Slots.Hash (B.Slot_List)
               & "&slot_number=" & B.Slot_Number
               & "&queue=" & B.Queue
               & "&hr=" & Resources.Hash (B.Hard)
               & "&sr=" & Hash (B.Soft)
               & """><img src=""/icons/arrow_right.png"" /></a>");

      HTML.Put_Cell (Data => B.PE);
      if not B.Slot_List.Is_Empty then
         Slots.Put_Cell (Data => B.Slot_List, Class => "right");
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
      return (Left.PE = Right.PE and then
                   Left.Slot_Number = Right.Slot_Number and then
                   Left.Slot_List = Right.Slot_List and then
                   Left.Hard = Right.Hard and then
                   Left.Soft = Right.Soft and then
       Left.Queue = Right.Queue);
   end "=";
end Bunches;
