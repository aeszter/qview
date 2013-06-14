with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded;
with Queues; use Queues;
with Host_Properties; use Host_Properties;

package Partitions is

   package Host_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 12);
   subtype Host_Name is Host_Names.Bounded_String;
   package Countable_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Host_Name,
                                                              "<"          => Host_Names."<",
                                                              "="          => Host_Names."=");
   package Countable_Maps is new Ada.Containers.Ordered_Maps (Key_Type     => Host_Name,
                                                              Element_Type => Natural,
                                                              "<"          => Host_Names."<",
                                                              "="          => "=");

   function Sum (Over : Countable_Maps.Map) return Natural;

   type Partition is record
      Used_Slots,
      Reserved_Slots          : Natural := 0;
      Total_Slots             : Countable_Maps.Map;
      Available_Hosts,
      Total_Hosts, Offline_Hosts, Reserved_Hosts, Used_Hosts, Suspended_Hosts : Countable_Sets.Set;
      Available_Slots,
      Suspended_Slots, Offline_Slots     : Countable_Maps.Map;
      Name                             : Unbounded_String;
      Properties                       : Set_Of_Properties;
   end record;
   type State is (total, available, used, reserved, suspended, offline);
   type State_Count is array (State) of Countable_Maps.Map;


   package Partition_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Partition);

   type Summarized_List is new Partition_Lists.List with
   record
     Summary : State_Count;
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
