with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Queues; use SGE.Queues;
with SGE.Parser;

package Queues is

   subtype Queue is SGE.Queues.Queue;
   procedure Put_Selected (Selector : not null access function (Q : Queue) return Boolean);

   procedure Append_List (Input_Nodes : SGE.Parser.Node_List);

private

   procedure Put_For_Maintenance (Q : Queue);

end Queues;
