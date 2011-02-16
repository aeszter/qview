with Ada.Containers.Doubly_Linked_Lists;
with DOM.Core; use DOM.Core;
with Queues; use Queues;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Hosts is

   type Job is record
      Master : Boolean;
      ID     : Positive;
   end record;


   package Job_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Job);
   subtype Job_List is Job_Lists.List;

   function New_Job (ID : Positive; Master : Boolean) return Job;
   function New_Job (ID : Positive; PE_Master : String) return Job;


   type Host is record
      Name       : Unbounded_String;
      Jobs       : Job_List;
      Properties : Queue;
   end record;

   procedure Append_List (Host_Nodes : Node_List);
   package Host_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Host);


   procedure Put (Cursor : Host_Lists.Cursor);
   Host_List : Host_Lists.List;


end Hosts;
