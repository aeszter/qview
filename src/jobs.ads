with Slurm.Jobs; use Slurm.Jobs;
with Slurm.Bunches;

package Jobs is

   Max_Name_Length : constant Positive := 25;

   function Name_As_HTML (J : Job) return String;

   procedure Put_Details (ID : Natural);
--     procedure Put_Time_List;
   procedure Put_List (Sort_By, Direction : String);
   procedure Put_Pending_List (Requirements : Slurm.Bunches.Set_Of_Requirements;
                               Sort_By, Direction : String);
   procedure Put_Global_List (Sort_By, Direction : String);
   procedure Put_Pending_List (Sort_By, Direction : String);
   procedure Put_Reserving_List (Sort_By, Direction : String);
   procedure Put_Running_List (Sort_By, Direction : String);
   procedure Put_User_List (User : String; Sort_By, Direction : String);
   procedure Put_Summary;
--     procedure Iterate (Process : not null access procedure (J : Job));
--
--     procedure Append_List (Nodes : Node_List; Fix_Posix_Prio : Boolean := False);
--     procedure Update_Messages (Nodes : Node_List);
--     procedure Create_Overlay (Nodes : Node_List);
--     procedure Apply_Overlay;
--     procedure Sort_By (Field : String; Direction : String);
--     procedure Update_Quota;
--     procedure Update_Status;
--     procedure Search_Queues;
--     procedure Prune_List (PE, Queue, Hard_Requests,
--                           Soft_Requests,
--                           Slot_Number, Slot_Ranges : Unbounded_String);

--     procedure Bunch (Result : out SGE.Bunches.List);

private
--     procedure Put (J : Job);
--     procedure Put_Time_Line (J : Job);
--     procedure Put_Res_Line (J : Job);
--     procedure Put_Prio_Line (J : Job);
--     procedure Put_Bunch_Line (J : Job);

--     procedure Put_Predecessor (ID : Natural);
   --  Output the job ID of a predecessor job as a link to the job, together with
   --  a suitable title
--     procedure Put_Successor (ID : Natural);
   --  Output the job ID of a successor job as a link to the job, together with
   --  a suitable title
--     procedure Put_Request (S : String);
   --  Output the name (or ID) of a successor or predecessor job, together with
   --  a suitable title
--     procedure Put_Usage (Kind : Usage_Type; Amount : Usage_Number);
   --  Output one item in a list of used resources, with type (name) and a number

end Jobs;
