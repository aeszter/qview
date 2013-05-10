with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Resources;
with Host_Properties; use Host_Properties;
with Parser; use Parser;

package Queues is

   type Queue is private;

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
      State, Q_Type         : String;
      Memory                : String;
      Cores                 : Natural;
      Network               : Resources.Network;
      SSD                   : Boolean;
      GPU                   : Boolean;
      Model                 : Resources.CPU_Model;
      Runtime               : Unbounded_String;
      Name                  : Unbounded_String;
      Long_Name             : Unbounded_String
     )
                       return Queue;


   function Precedes_By_Resources (Left, Right : Queue) return Boolean;

   function Get_Properties (Q : Queue) return Set_Of_Properties;
   function Get_Name (Q : Queue) return Unbounded_String;
   function Get_Name (Q : Queue) return String;
   function Get_Slot_Count (Q : Queue) return Natural;
   function Get_Used_Slots (Q : Queue) return Natural;
   function Get_Reserved_Slots (Q : Queue) return Natural;
   function Get_Free_Slots (Q : Queue) return Natural;
   function Is_Offline (Q : Queue) return Boolean;
   function Is_Suspended (Q : Queue) return Boolean;

   function Has_Error (Q : Queue) return Boolean;
   function Has_Disabled (Q : Queue) return Boolean;
   function Has_Unreachable (Q : Queue) return Boolean;
   function Is_Batch (Q : Queue) return Boolean;
   function Is_Interactive (Q : Queue) return Boolean;
   function Is_Parallel (Q : Queue) return Boolean;

   procedure Put_Selected (Selector : not null access function (Q : Queue) return Boolean);

private

   type State_Flag is (alarm, disabled, error, unreachable, old);
   type Type_Flag is (B, I, P);
   type State_Array is array (State_Flag) of Boolean;
   type Type_Array is array (Type_Flag) of Boolean;

   type Queue is record
      Used, Reserved, Total : Natural;
      Name, Long_Name       : Unbounded_String;
      Properties            : Set_Of_Properties;
      State                 : State_Array := (others => False);
      Q_Type                : Type_Array := (others => False);
   end record;

   package Queue_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Queue);
   package Sorting_By_Resources is
     new Queue_Lists.Generic_Sorting ("<" => Precedes_By_Resources);

   procedure Put_For_Maintenance (Cursor : Queue_Lists.Cursor);

   List : Queue_Lists.List;
   List_Cursor : Queue_Lists.Cursor := Queue_Lists.No_Element;

end Queues;
