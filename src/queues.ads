with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Resources;
with DOM.Core; use DOM.Core;

package Queues is
   type Gigs is delta 0.001 digits 7;


   type Queue is record
      Used, Reserved, Total : Natural;
      Suspended, Offline    : Boolean;
      Network               : Resources.Network;
      Model                 : Resources.CPU_Model;
      Memory                : Gigs;
      Cores                 : Positive;
      Runtime               : Unbounded_String;
      Name                  : Unbounded_String;
   end record;

   procedure Sort;
   --  Sort the queue list by resources
   procedure Rewind;
   --  rewind the queue list, i.e. point the memory pointer at the first queue
   function Empty return Boolean;
   --  is the queue list empty?
   function Next return Queue;
   --  advance the memory pointer and retrieve the current queue
   --  if the memory pointer points at the last element, or is No_Element, then
   --  a Constraint_Error is propagated
   function At_End return Boolean;
   --  is there a next queue? If At_End returns False, Next will return a Queue
   function Current return Queue;
   --  retrieve the current queue without changing the memory pointer
   procedure Append_List (Input_Nodes : Node_List);
   function New_Queue (Used, Reserved, Total : Natural;
                       State                 : String;
                       Memory                : String;
                       Cores                 : Natural;
                       Network               : Resources.Network;
                       Model                 : Resources.CPU_Model;
                       Runtime               : Unbounded_String;
                      Name : Unbounded_String)
                       return Queue;

   function To_Gigs (Memory : String) return Gigs;

   function Precedes_By_Resources (Left, Right : Queue) return Boolean;


   package Queue_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Queue);
   package Sorting_By_Resources is
     new Queue_Lists.Generic_Sorting ("<" => Precedes_By_Resources);
private
   List : Queue_Lists.List;
   List_Cursor : Queue_Lists.Cursor := Queue_Lists.No_Element;
end Queues;
