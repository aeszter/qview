with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with SGE.Hosts; use SGE.Hosts;

package Nodes is
--     subtype Host is SGE.Hosts.Host;

   procedure Put_All;
   procedure Put_Details;
--     procedure Put_Selected (Selector : not null access function (H : Host) return Boolean);
private

--     procedure Put_Queue (Q : Queue_Pointer);

--     procedure Put (H : SGE.Hosts.Host);
--     procedure Put_Details (H : SGE.Hosts.Host);
--     procedure Put_Jobs (J : SGE.Hosts.Job);
--     procedure Put_For_Maintenance (H : SGE.Hosts.Host);
end Nodes;
