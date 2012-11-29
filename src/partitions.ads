with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Queues; use Queues;
with Host_Properties; use Host_Properties;

package Partitions is

   type Partition is record
      Used_Slots, Used_Hosts, Reserved_Slots, Reserved_Hosts,
      Total_Slots, Total_Hosts         : Natural := 0;
      Available_Slots, Available_Hosts : Natural := 0;
      Suspended_Slots, Suspended_Hosts, Offline_Slots, Offline_Hosts  : Natural := 0;
      Name                             : Unbounded_String;
      Properties                       : Set_Of_Properties;
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

   function To_String (Source : State) return String;
end Partitions;
