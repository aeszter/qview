with Ada.Containers.Doubly_Linked_Lists;
with Jobs; use Jobs; use Jobs.Job_Lists;

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
      J := Element (Cursor);

      --  Create Bunch according to first Job
      B := New_Bunch (J);
      while Cursor /= No_Element loop
         J := Element (Cursor);
         --  New Bunch?
         if not (B.PE = J.PE and then
                   B.Slots = J.Slot_Number and then
                   Resources.Equal (B.Hard, J.Hard) and then
                   Resources.Equal (B.Soft, J.Soft) and then
                   B.Queue = J.Queue) then
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
   end Build_List;

   -------------------
   -- New_Bunch --
   -------------------

   function New_Bunch (J : Job) return Bunch is
      B : Bunch;
   begin
      B.PE := J.PE;
      B.Slots := J.Slot_Number;
      B.Queue := J.Queue;
      B.Hard := J.Hard;
      B.Soft := J.Soft;
      B.Total := 0;
      B.On_Hold := 0;
      B.Waiting := 0;
      B.Error := 0;
      return B;
   end New_Bunch;

end Bunches;
