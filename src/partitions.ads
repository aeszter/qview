with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Resources;
with Queues; use Queues;

package Partitions is

   type Partition is record
      Used, Reserved, Total : Natural;
      Available             : Natural;
      Suspended, Offline    : Natural;
      Network               : Resources.Network;
      Model                 : Resources.CPU_Model;
      Memory                : Gigs;
      Cores                 : Positive;
      Runtime, Name         : Unbounded_String;
   end record;

   package Partition_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Partition);

   procedure Build_List (Q_List : in out Queues.Queue_Lists.List;
                         Part_List : out Partition_Lists.List);
   function New_Partition (Q : Queue) return Partition;
   procedure Put (Partition : Partitions.Partition_Lists.Cursor);
   function Model_As_String (P : Partition) return String;

   function "=" (Left : Partition; Right : Queue) return Boolean;
   function "=" (Left : Queue; Right : Partition) return Boolean;
end Partitions;
