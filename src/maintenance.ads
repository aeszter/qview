with Nodes;

package Maintenance is
   procedure Put_All;
private
--     procedure Read_Lightsout_Information;
   procedure Put_Error_Messages;
   procedure Put_Header;
   procedure Put_High_Load;
   procedure Put_Low_Load;
   procedure Put_Swapping_Hosts;
   procedure Put_Error_Queues;
   procedure Put_Drained;
   procedure Put_Unreachable;
   procedure Put_Unusual_Queues;
   procedure Put_Offline;
   procedure Put_Multi_Queues;
   procedure Put_No_Queues;
   procedure Put_Old_Config;

   function High_Load (N : Nodes.Node) return Boolean;
   function Low_Load (N : Nodes.Node) return Boolean;
--     function High_Swap (H : Hosts.Host) return Boolean;
--     function No_Queue (H : Hosts.Host) return Boolean;
--     function Multi_Queue (H : Hosts.Host) return Boolean;
--     function Old_Config (Q : Queues.Queue) return Boolean;
--     function In_Error_State (Q : Queues.Queue) return Boolean;
   function Reachable_Disabled (N : Nodes.Node) return Boolean;
   function Unreachable_Enabled (N : Nodes.Node) return Boolean;
   function Unreachable_Disabled (N : Nodes.Node) return Boolean;
--     function Unusual_Type (Q : Queues.Queue) return Boolean;
end Maintenance;
