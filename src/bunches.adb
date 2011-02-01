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
      Slots : Natural;
   begin
      Sorting_By_Resources.Sort (Job_List);
      Cursor := Job_List.First;
      J := Element (Cursor);

      --  Create Bunch according to first Job
      B := New_Bunch (J);
      while Cursor /= No_Element loop
         J := Element (Cursor);
         --  New Bunch?
         if not (B.Slots = J.Slots and then
           Resources.Equal (B.Hard, J.Hard) and then
           Resources.Equal (B.Soft, J.Soft) and then
           B.Queue = J.Queue) then
            --  Yes. Store previous one.
            Bunch_List.Append (B);
            B := New_Bunch (J);
         end if;

         --  Update totals
         Slots := Integer'Value (To_String (J.Slots));
         B.Total_Slots := B.Total_Slots + Slots;
         if On_Hold (J) then
            B.Slots_On_Hold := B.Slots_On_Hold + Slots;
         else
            B.Slots_Waiting := B.Slots_Waiting + Slots;
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
      B.Slots := J.Slots;
      B.Queue := J.Queue;
      B.State := J.State;
      B.Hard := J.Hard;
      B.Soft := J.Soft;
      return B;
   end New_Bunch;

end Bunches;
