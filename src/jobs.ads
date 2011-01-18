with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Core; use DOM.Core;
with Ada.Calendar; use Ada.Calendar;

package Jobs is

   type Job_State is (unknown, dt, dr, Eqw, t, r, Rr, qw, hqw);

   type Job is record
      --  basic attributes
      Number             : Unbounded_String; -- Job ID
      Full_Name          : Unbounded_String; -- Job name
      Name               : Unbounded_String; -- Job name, truncated to Max_J_Name_Length
      Name_Truncated     : Boolean;          -- Whether Full_Name and Name differ
      Owner              : Unbounded_String; -- User whom this job belongs to
      Priority           : Unbounded_String; -- Numerical priority
      State              : Job_State;
      Slots              : Unbounded_String; -- how many slots/CPUs to use
      PE                 : Unbounded_String; -- Parallel environment
      Submission_Time    : Time;    -- when submitted

      --  qstat -ext
      CPU, Mem, IO       : Float;
      Override_Tickets   : Natural;
      Share_Tickets      : Natural;
      Functional_Tickets : Natural;

      --  qstat -urg
      Urgency            : Natural;
      Resource_Contrib   : Natural;
      Waiting_Contrib    : Natural;

      --  qstat -pri
      Posix_Priority     : Natural;
   end record;

   function State_As_String (J : Job) return String;
   function Name_As_HTML (J : Job) return String;

   function New_Job (Number, Name, Owner, Priority, State,
                     Slots, PE : Unbounded_String; Submission_Time : Time;
                     CPU, Mem, IO : Float := 0.0;
                     Override_Tickets, Share_Tickets, Functional_Tickets : Natural := 0;
                     Urgency, Resource_Contrib, Waiting_Contrib          : Natural := 0;
                     Posix_Priority                                      : Integer := 0)
                     return Job;

   procedure Append_List (List : Node_List);
   procedure Sort_By (Field : String);
   function Precedes_By_Name (Left, Right : Job) return Boolean;
   function Precedes_By_Number (Left, Right : Job) return Boolean;
   function Precedes_By_Owner (Left, Right : Job) return Boolean;
   function Precedes_By_Priority (Left, Right : Job) return Boolean;
   function Precedes_By_Submission_Time (Left, Right : Job) return Boolean;
   function Precedes_By_Slots (Left, Right : Job) return Boolean;
   function Precedes_By_State (Left, Right : Job) return Boolean;
   function Same (Left, Right : Job) return Boolean;

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job, "=" => Same);

   package Sorting_By_Name is
     new Job_Lists.Generic_Sorting
       ("<" => Precedes_By_Name);

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


   Max_Name_Length : constant Positive := 20;
   Job_List : Job_Lists.List;
end Jobs;
