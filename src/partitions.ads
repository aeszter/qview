with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Queues; use Queues;
with Host_Properties; use Host_Properties;

package Partitions is

   type Partition is record
      Used, Reserved, Total : Natural;
      Available             : Natural;
      Suspended, Offline    : Natural;
      Name                  : Unbounded_String;
      Properties            : Set_Of_Properties;
   end record;
   type State is (total, available, used, reserved, suspended, offline);
   type State_Count is array (State) of Natural;


   package Partition_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Partition);

   type Summarized_List is new Partition_Lists.List with
   record
     Summary : State_Count := (others => 0);
   end record;

   procedure Build_List;
   function New_Partition (Q : Queue) return Partition;
   procedure Put (Partition : Partitions.Partition_Lists.Cursor);
   procedure Put_List;
   procedure Put_Summary;

   function "=" (Left : Partition; Right : Queue) return Boolean;
   function "=" (Left : Queue; Right : Partition) return Boolean;
private
   List : Summarized_List;
end Partitions;
