with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Core; use DOM.Core;

package Jobs is

   type Job is record
      Number          : Unbounded_String; -- Job ID
      Full_Name       : Unbounded_String; -- Job name
      Name            : Unbounded_String; -- Job name, truncated to Max_J_Name_Length
      Owner           : Unbounded_String; -- User whom this job belongs to
      Priority        : Unbounded_String; -- Numerical priority
      State           : Unbounded_String; -- r(unning), qw(aiting) etc.
      Slots           : Unbounded_String; -- how many slots/CPUs to use
      PE              : Unbounded_String; -- Parallel environment
      Submission_Time : Unbounded_String; -- when submitted
   end record;

   function New_Job (Number, Full_Name, Name, Owner, Priority, State,
                     Slots, PE, Submission_Time : Unbounded_String)
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
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Submission_time);
   package Sorting_By_Slots is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_Slots);
   package Sorting_By_State is
     new Job_Lists.Generic_Sorting ("<" => Precedes_By_State);


   Max_Name_Length : constant Positive := 20;
   Job_List : Job_Lists.List;
end Jobs;
