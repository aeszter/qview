package body Jobs is

   -------------
   -- New_Job --
   -------------

   function New_Job
     (Number, Full_Name, Name, Owner, Priority, State,
                     Slots, PE, Submission_Time : Unbounded_String)
      return Job
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (True, "New_Job unimplemented");
      raise Program_Error with "Unimplemented function New_Job";
      return New_Job (Number, Full_Name, Name, Owner, Priority, State, Slots,
         PE, Submission_Time);
   end New_Job;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (List : Node_List) is
   begin
       for Index in 1 .. Length (List) loop
--         Ada.Text_IO.Put ("<tr>");
         N        := Item (List, Index - 1);
         Children := Child_Nodes (N);
         for Ch_Index in 0 .. Length (Children) - 1 loop
            C := Item (Children, Ch_Index);
            if Name (C) = "JB_job_number" then
               J_Number := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JAT_prio" then
               J_Priority := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_name" then
               J_Full_Name := To_Unbounded_String (Value (First_Child (C)));
               if Length (J_Full_Name) > Max_J_Name_Length then
                  J_Name := Head (Source => J_Full_Name,
                                  Count  => Max_J_Name_Length);
               else
                  J_Name := J_Full_Name;
               end if;
            elsif Name (C) = "JB_owner" then
               J_Owner := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "state" then
               J_State := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "JB_submission_time" then
               J_Submission_Time :=
               To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "slots" then
               J_Slots := To_Unbounded_String (Value (First_Child (C)));
            elsif Name (C) = "requested_pe" then
               J_PE := To_Unbounded_String (Value (First_Child (C)));
            end if;
         end loop;
         Job_List.Append (New_Job (Number          => J_Number,
                                   Full_Name       => J_Full_Name,
                                   Name            => J_Name,
                                   Owner           => J_Owner,
                                   Priority        => J_Priority,
                                   State           => J_State,
                                   Slots           => J_Slots,
                                   PE              => J_PE,
                                   Submission_Time => J_Submission_Time));
--         HTML.Put_Cell (Data => J_Number, Link_Param => "job_id");
--         HTML.Put_Cell (Data => J_Owner, Link_Param => "user");
--         HTML.Put_Cell (Data => J_Name);
--         HTML.Put_Cell (Data => J_Priority);
--         HTML.Put_Cell (Data => J_Submission_Time);
--         HTML.Put_Cell (Data => J_Slots, Tag => "td class=""right""");
--         HTML.Put_Cell (Data => J_State);
--         Ada.Text_IO.Put ("</tr>");
      end loop;
  end Append_List;

end Jobs;
