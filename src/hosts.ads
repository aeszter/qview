with Ada.Containers.Doubly_Linked_Lists;
with DOM.Core; use DOM.Core;
with Queues; use Queues;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Hosts is

   type Job is record
      Master : Boolean;
      ID     : Positive;
   end record;

   type Fixed is delta 0.01 digits 5 range 0.0 .. 100.0;
   type Percent is range 0 .. 100;

   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job);
   subtype Job_List is Job_Lists.List;

   function New_Job (ID : Positive; Master : Boolean) return Job;
   function New_Job (ID : Positive; PE_Master : String) return Job;


   type Host is record
      Name       : Unbounded_String;
      Jobs       : Job_List;
      Properties : Queue;
      Load       : Fixed;
      Mem_Used   : Gigs;
      Swap_Total : Gigs;
      Swap_Used  : Gigs;
   end record;

   procedure Append_List (Host_Nodes : Node_List);
   package Host_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Host);


   procedure Put (Cursor : Host_Lists.Cursor);
   procedure Put_Jobs (Cursor : Job_Lists.Cursor);

   function Load_Per_Core (H : Host) return Fixed;
   function Mem_Ratio (H : Host) return Fixed;
   function Mem_Percentage (H : Host) return Percent;
   function Swap_Ratio (H : Host) return Fixed;
   function Swap_Percentage (H : Host) return Percent;

   Host_List : Host_Lists.List;


end Hosts;
