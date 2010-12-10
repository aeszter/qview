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

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job);
end Jobs;
