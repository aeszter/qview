with Slurm.Nodes; use Slurm.Nodes;
with Slurm.Partitions;

package Nodes is
--     subtype Host is SGE.Hosts.Host;

   procedure Put_All;
   procedure Put_Details (Name : String);
   procedure Put_List (List : Slurm.Nodes.List);
   function Explain_State (S : states) return String;

--     procedure Put_Selected (Selector : not null access function (H : Host) return Boolean);
private

   procedure Put_Partition (P : Slurm.Partitions.Partition);

--     procedure Put (H : SGE.Hosts.Host);
--     procedure Put_Details (H : SGE.Hosts.Host);
   procedure Put_Jobs (ID : Positive);
--     procedure Put_For_Maintenance (H : SGE.Hosts.Host);
end Nodes;
