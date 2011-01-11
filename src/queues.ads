with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Resources;

package Queues is
   type Gigs is digits 1;

   type Queue is record
      Used, Reserved, Total : Natural;
      Network               : Resources.Network;
      Memory                : Gigs;
      Cores                 : Positive;
      Runtime               : Unbounded_String;
   end record;

   function New_Queue (Used, Reserved, Total : Natural;
                       Memory, Cores         : String;
      Network               : Resources.Network;
      Runtime               : Unbounded_String)
                       return Queue;



   package Queue_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Queue);
end Queues;
