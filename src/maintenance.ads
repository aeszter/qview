with Hosts;
with Queues;

package Maintenance is
   procedure Put_All;
private
   procedure Read_Lightsout_Information;
   procedure Put_Header;
   procedure Put_High_Load_Hosts;
   procedure Put_Low_Load_Hosts;
   procedure Put_Swapping_Hosts;
   procedure Put_Error_Queues;
   procedure Put_Disabled_Queues;
   procedure Put_Unreachable_Queues;
   procedure Put_Unusual_Queues;


   function High_Load (H : Hosts.Host) return Boolean;
   function Low_Load (H : Hosts.Host) return Boolean;
   function High_Swap (H : Hosts.Host) return Boolean;
   function In_Error_State (Q : Queues.Queue) return Boolean;
   function Reachable_Disabled (Q : Queues.Queue) return Boolean;
   function Unreachable_Enabled (Q : Queues.Queue) return Boolean;
   function Unusual_Type (Q : Queues.Queue) return Boolean;
end Maintenance;
