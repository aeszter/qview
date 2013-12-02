with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SGE.Hosts; use SGE.Hosts;

package Hosts is
   subtype Host is SGE.Hosts.Host;

   procedure Put_All;
   procedure Put_Selected (Selector : not null access function (H : Host) return Boolean);
private

--   procedure Put_Queue (Q : SGE.Hosts.Queue);

   procedure Put (H : SGE.Hosts.Host);
   procedure Put_Jobs (J : SGE.Hosts.Job);
   procedure Put_For_Maintenance (H : SGE.Hosts.Host);
end Hosts;
