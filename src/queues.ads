with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Resources;

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
   end record;

   function New_Queue (Used, Reserved, Total : Natural;
      State                 : String;
      Memory                : String;
      Cores                 : Natural;
      Network               : Resources.Network;
      Model                 : Resources.CPU_Model;
      Runtime               : Unbounded_String)
                       return Queue;

   function To_Gigs (Memory : String) return Gigs;

   function Precedes_By_Resources (Left, Right : Queue) return Boolean;


   package Queue_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Queue);
   package Sorting_By_Resources is
      new Queue_Lists.Generic_Sorting ("<" => Precedes_By_Resources);
end Queues;
