with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Resources;
with SGE.Host_Properties; use SGE.Host_Properties;
with Parser; use Parser;
with SGE.Queues; use SGE.Queues;

package Queues is

   subtype Queue is SGE.Queues.Queue;
   procedure Put_Selected (Selector : not null access function (Q : Queue) return Boolean);

private

   procedure Put_For_Maintenance (Q : Queue);

end Queues;
