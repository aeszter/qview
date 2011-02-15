with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Core; use DOM.Core;
with Ada.Calendar; use Ada.Calendar;
with Resources;
with Slots;
with Utils; use Utils;

package Jobs is

   type Job_State is (unknown, dt, dr, Eqw, t, r, Rr, Rq, qw, hqw);
   type Fixed is delta 0.0001 digits 5;


   type Job is record
      --  basic attributes
      Number             : Integer; -- Job ID
      Full_Name          : Unbounded_String; -- Job name
      Name               : Unbounded_String; -- Job name, truncated to Max_J_Name_Length
      Name_Truncated     : Boolean;          -- Whether Full_Name and Name differ
      Owner              : Unbounded_String; -- User whom this job belongs to
      Group              : Unbounded_String;
      Account            : Unbounded_String;

      Priority           : Fixed; -- Numerical priority
      State              : Job_State;
      Slot_Number        : Unbounded_String; -- how many slots/CPUs to use
      PE                 : Unbounded_String; -- Parallel environment
      Submission_Time    : Time;    -- when submitted
      Project            : Unbounded_String;
      Department         : Unbounded_String;
      Job_Array          : Unbounded_String;
      Notify             : Tri_State;


      --  File related stuff
      Exec_File          : Unbounded_String;
      Script_File        : Unbounded_String;
      Directory          : Unbounded_String;
      Reserve            : Tri_State;
      Merge_Std_Err      : Tri_State;


      --  qstat -ext
      CPU, Mem, IO       : Float;
      Override_Tickets   : Natural;
      Share_Tickets      : Natural;
      Functional_Tickets : Natural;

      --  qstat -urg
      Urgency          : Fixed;
      Resource_Contrib : Natural;
      Waiting_Contrib  : Natural;

      --  qstat -pri
      Posix_Priority   : Natural;

      --  resources used for Bunching jobs
      Queue            : Unbounded_String;
      Hard, Soft       : Resources.Resource_Lists.List;

      Slot_List        : Slots.Slot_Lists.List;
      Queue_List       : String_Lists.List;
      Message_List     : String_Lists.List;
      Task_List        : String_Lists.List;

      Std_Out_Paths    : String_Lists.List;
      Std_Err_Paths    : String_Lists.List;


   end record;

   function State_As_String (J : Job) return String;
   function To_State (State : String) return Job_State;
   function Name_As_HTML (J : Job) return String;
   function On_Hold (J : Job) return Boolean;
   function Has_Error (J : Job) return Boolean;

   function End_Time (J : Job) return Time;
   function Remaining_Time (J : Job) return Duration;

   function New_Job (List : Node_List) return Job;
   procedure Extract_Resource_List (J : in out Job; Resource_Nodes : Node_List);
   procedure Extract_Queue_List (J : in out Job; Destin_Nodes : Node_List);
   procedure Extract_Tasks (J : in out Job; Task_Nodes : Node_List);
   procedure Extract_PE_Range (J : in out Job; Children : Node_List);
   procedure Extract_Paths (Path_List  : in out String_Lists.List;
                            List_Nodes  : Node_List);

   procedure Append_List (List : Node_List);
   procedure Sort_By (Field : String; Direction : String);
   function Precedes_By_Name (Left, Right : Job) return Boolean;
   function Precedes_By_Number (Left, Right : Job) return Boolean;
   function Precedes_By_Owner (Left, Right : Job) return Boolean;
   function Precedes_By_Priority (Left, Right : Job) return Boolean;
   function Precedes_By_Submission_Time (Left, Right : Job) return Boolean;
   function Precedes_By_Slots (Left, Right : Job) return Boolean;
   function Precedes_By_State (Left, Right : Job) return Boolean;
   function Precedes_By_CPU_Used (Left, Right : Job) return Boolean;
   function Precedes_By_Memory_Used (Left, Right : Job) return Boolean;
   function Precedes_By_IO_Used (Left, Right : Job) return Boolean;
   function Precedes_By_Override (Left, Right : Job) return Boolean;
   function Precedes_By_Share (Left, Right : Job) return Boolean;
   function Precedes_By_Functional (Left, Right : Job) return Boolean;
   function Precedes_By_Urgency (Left, Right : Job) return Boolean;
   function Precedes_By_Waiting_Contrib (Left, Right : Job) return Boolean;
   function Precedes_By_Resource_Contrib (Left, Right : Job) return Boolean;
   function Precedes_By_Posix_Priority (Left, Right : Job) return Boolean;
   function Precedes_By_End (Left, Right : Job) return Boolean;

   function Precedes_By_Resources (Left, Right : Job) return Boolean;

   function Same (Left, Right : Job) return Boolean;

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job, "=" => Same);

   package Sorting_By_Name is
     new Job_Lists.Generic_Sorting
       ("<" => Precedes_By_Name);

   procedure Put (Cursor : Job_Lists.Cursor);

   package Sorting_By_Number is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Number);
   package Sorting_By_Owner is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Owner);
   package Sorting_By_Priority is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Priority);
   package Sorting_By_Submission_Time is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Submission_Time);
   package Sorting_By_Slots is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Slots);
   package Sorting_By_State is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_State);
   package Sorting_By_CPU_Used is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_CPU_Used);
   package Sorting_By_Memory_Used is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Memory_Used);
   package Sorting_By_IO_Used is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_IO_Used);
   package Sorting_By_Override is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Override);
   package Sorting_By_Share is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Share);
   package Sorting_By_Functional is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Functional);
   package Sorting_By_Urgency is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Urgency);
   package Sorting_By_Resource_Contrib is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Resource_Contrib);
   package Sorting_By_Waiting_Contrib is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Waiting_Contrib);
   package Sorting_By_Posix_Priority is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Posix_Priority);
   package Sorting_By_End is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_End);

   package Sorting_By_Resources is
      new Job_Lists.Generic_Sorting ("<" => Precedes_By_Resources);


   Max_Name_Length : constant Positive := 20;
   Job_List : Job_Lists.List;
end Jobs;
