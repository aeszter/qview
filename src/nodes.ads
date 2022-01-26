with Slurm.Nodes; use Slurm.Nodes;
with Slurm.Node_Properties;
with Slurm.Partitions;

package Nodes is
   subtype Node is Slurm.Nodes.Node;

   procedure Put_All;
   procedure Put_Details (Name : String);
   procedure Put_List (List : Slurm.Nodes.List);
   procedure Put_For_Maintenance (List : Slurm.Nodes.List);
   procedure Put_List (Properties : Slurm.Node_Properties.Set_Of_Properties);
   procedure Put_Equivalent (Name : String);
   function Explain_State (S : states) return String;

   procedure Init (Properties : out Slurm.Node_Properties.Set_Of_Properties;
                   GRES, TRES, Memory, CPUs, Features : String);
   procedure Put_Selected (Selector : not null access function (N : Node) return Boolean);
private

   procedure Put_Partition (P : Slurm.Partitions.Partition);

--     procedure Put (H : SGE.Hosts.Host);
   procedure Put_Jobs (ID : Positive; N : Node);
--     procedure Put_For_Maintenance (H : SGE.Hosts.Host);
end Nodes;
