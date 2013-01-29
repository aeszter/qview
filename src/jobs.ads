with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with Resources;
with Parser; use Parser;
with Ranges;
with Utils; use Utils;

package Jobs is

   type Job_State is (unknown, dt, dr, Eqw, t, r, Rr, Rq, qw, hqw, ERq, hr);
   type State_Count is array (Job_State) of Natural;
   type Usage_Type is (cpu, mem, io, iow, vmem, maxvmem, submission_time, priority);
   type Usage_Number is delta 0.00001 digits 18;
   type Usage_Integer is range 0 .. 10 ** 12;
   type Usage is array (Usage_Type) of Usage_Number;
   type Posix_Priority_Type is range -1_023 .. 1_024;

   type Job is private;

   function State_As_String (J : Job) return String;
   function To_String (State : Job_State) return String;
   function To_State (State : String) return Job_State;
   function To_Memory (Amount : Usage_Integer) return String;
   --  Purpose: Compose a memory quantity consisting of a number and a unit
   --  Returns: A string of the form xx MB, where xx is a number not exceeding
   --           five digits, and MB is a unit (actually kB, MB, or GB)

   function Name_As_HTML (J : Job) return String;
   function On_Hold (J : Job) return Boolean;
   function Has_Error (J : Job) return Boolean;

   function End_Time (J : Job) return Time;
   function Remaining_Time (J : Job) return Duration;
   function Get_Task_Count (J : Job) return Natural;
   function Get_ID (J : Job) return String;
   function Get_PE (J : Job) return Unbounded_String;
   function Get_Slot_List (J : Job) return Ranges.Range_Lists.List;
   function Get_Slot_Number (J : Job) return Unbounded_String;
   function Get_Queue (J : Job) return Unbounded_String;
   function Get_Hard_Resources (J : Job) return Resources.Hashed_List;
   function Get_Soft_Resources (J : Job) return Resources.Hashed_List;

   -------------
   -- New_Job --
   --  Purpose: Create a new job and populate its fields from a given list
   --  of XML nodes
   --  Parameter List: A list of XML nodes containing information about the job
   --  Returns: The newly created job
   -------------

   function New_Job (List : Node_List) return Job;

   ----------------
   -- Update_Job --
   --  Purpose: Populate the fields of a given job from a list of XML nodes
   --  Parameter J: The job to update
   --  Parameter List: A list of XML nodes containing information about the job
   ----------------
   procedure Update_Job (J : in out Job; List : Node_List);
   procedure Extract_Resource_List (J              : in out Job;
                                    Resource_Nodes : Node_List;
                                    Soft           : Boolean := False);
   procedure Extract_Queue_List (J : in out Job; Destin_Nodes : Node_List);
   procedure Extract_Hold_ID_List (ID_List         : in out Utils.ID_List;
                                       Sub_Nodes : Node_List);
   procedure Extract_Tasks (J : in out Job; Task_Nodes : Node_List);
   procedure Extract_PE_Range (J : in out Job; Children : Node_List);
   procedure Extract_Paths (Path_List  : in out String_Lists.List;
                            List_Nodes  : Node_List);
   procedure Extract_Args (J : in out Job;
                           Arg_Nodes : Node_List);

   -----------------
   -- Append_List --
   --  Purpose: Read Jobs from a given list of (DOM) Nodes and populate the List
   --  accordingly
   -----------------

   procedure Append_List (Nodes : Node_List);

   -----------------
   -- Append_List --
   --  Purpose: Read Jobs from a given list of (DOM) Nodes and populate the List
   --  accordingly. Jobs are selected according to certain criteria
   -----------------
   procedure Append_List (Nodes                    : Node_List;
                          PE, Queue, Hard_Requests,
                          Soft_Requests,
                          Slot_Number, Slot_Ranges : Unbounded_String);

   ------------------------------
   -- Update_List_From_Qstat_J --
   --  Purpose: Get more information on existing jobs;
   --  call qstat -j for each Job in List, filling in information for
   --  previously empty fields
   ------------------------------

   --  Is this still needed?
   procedure Update_List_From_Qstat_J;

   ----------------
   -- Prune_List --
   --  Purpose: Prune the Job_List, keeping only Jobs that fulfill
   --          the given requirements
   --  Parameter: PE: Parallel environment required
   --  Parameter Queue: Queue required
   --  Parameter Hard_Requests: List of hard resource requests
   --  Parameter Soft_Requests: List of soft resource requests
   ----------------
   --   procedure Prune_List (PE, Queue, Hard_Requests, Soft_Requests : String);
   --  outdated. move functionality to Append_List

   ----------------
   -- Prune_List --
   --  Purpose: Prune the Job_List, keeping only Jobs that fulfill
   --          the given requirements
   --  Parameter Slots: Hash of the allowed slot ranges
   ----------------
   procedure Prune_List_By_Slots (Slots : String);
   --  outdated. move functionality to Append_List. Does GPS notice this?

   procedure Put_Summary;
   procedure Put_List;
   procedure Put_Time_List;
   procedure Put_Bunch_List;

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

   procedure Update_Job_From_Qstat_J (J : in out Job);
   --  Purpose: Get more information on an existing job;
   procedure Update_Status;
   --  Purpose: Update all jobs' status
   procedure Search_Queues;
   --  Look for slots occupied by a job

   procedure Put_Details;

   procedure Sort;
   --  Sort the job list by resources
   procedure Rewind;
   --  rewind the job list, i.e. point the memory pointer at the first job
   function Empty return Boolean;
   --  is the job list empty?
   function Next return Job;
   --  advance the memory pointer and retrieve the current job
   --  if the memory pointer points at the last element, or is No_Element, then
   --  a Constraint_Error is propagated
   function At_End return Boolean;
   --  is there a next job? If At_End returns False, Next will return a Job
   function Current return Job;
   --  retrieve the current job without changing the memory pointer


   Max_Name_Length : constant Positive := 20;

private
   type Job is record
      --  basic attributes
      Number               : Integer; -- Job ID
      Task_IDs             : Ranges.Step_Range_List;
      Full_Name            : Unbounded_String; -- Job name
      Name                 : Unbounded_String; -- Job name, truncated to Max_J_Name_Length
      Name_Truncated       : Boolean;          -- Whether Full_Name and Name differ
      Owner                : Unbounded_String; -- User whom this job belongs to
      Group                : Unbounded_String;
      Account              : Unbounded_String;

      Priority             : Fixed; -- Numerical priority
      State                : Job_State;
      Slot_Number          : Unbounded_String; -- how many slots/CPUs to use
      PE                   : Unbounded_String; -- Parallel environment
      Submission_Time      : Time;    -- when submitted
      Project              : Unbounded_String;
      Department           : Unbounded_String;
      Job_Array            : Unbounded_String;
      Notify               : Tri_State;
      JAT_Usage, PET_Usage : Usage := (others => 0.0);
      Predecessors         : Utils.ID_List;
      Successors           : Utils.ID_List;
      Predecessor_Request  : Utils.String_List;


      --  File related stuff
      Exec_File          : Unbounded_String;
      Script_File        : Unbounded_String;
      Directory          : Unbounded_String;
      Reserve            : Tri_State;
      Merge_Std_Err      : Tri_State;
      Args               : Utils.String_List;


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
      Posix_Priority   : Posix_Priority_Type;

      --  qhost -j
      Detected_Queues  : String_Sets.Set;

      --  resources used for Bunching jobs
      Queue            : Unbounded_String;
      Hard, Soft       : Resources.Hashed_List;

      Slot_List        : Ranges.Range_Lists.List;
      Queue_List       : String_Lists.List;
      Message_List     : String_Lists.List;
      Task_List        : String_Lists.List;

      Std_Out_Paths    : String_Lists.List;
      Std_Err_Paths    : String_Lists.List;
   end record;

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job, "=" => Same);

   function Find_Job (ID : Natural) return Job_Lists.Cursor;
   --  if the job list contains a job with the given ID, return a cursor
   --  pointing there;
   --  otherwise, return No_Element


   -----------------
   -- Get_Summary --
   --  Purpose: Count the number of jobs per state from the List
   -----------------
   procedure Get_Summary (Tasks, Slots : out State_Count);

   package Sorting_By_Name is
     new Job_Lists.Generic_Sorting
       ("<" => Precedes_By_Name);


   procedure Put (Cursor : Job_Lists.Cursor);
   procedure Put_Time_Line (Pos : Job_Lists.Cursor);
   procedure Put_Res_Line (Pos : Job_Lists.Cursor);
   procedure Put_Bunch_Line (Pos : Job_Lists.Cursor);

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


   List : Job_Lists.List;
   List_Cursor : Job_Lists.Cursor := Job_Lists.No_Element;

   procedure Update_Status (J : in out Job);
   --  Purpose: Read the job's status from an appropriate source
   --  (such as a qstat -u call)
   procedure Put_Predecessor (Position : Utils.ID_Lists.Cursor);
   --  Output the job ID of a predecessor job as a link to the job, together with
   --  a suitable title
   procedure Put_Successor (Position : Utils.ID_Lists.Cursor);
   --  Output the job ID of a successor job as a link to the job, together with
   --  a suitable title
   procedure Put_Request (Position : Utils.String_Lists.Cursor);
   --  Output the name (or ID) of a successor or predecessor job, together with
   --  a suitable title
   procedure Put_Usage (Kind : Usage_Type; Amount : Usage_Number);
   --  Output one item in a list of used resources, with type (name) and a number

end Jobs;
