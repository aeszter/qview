with Hosts;

package Maintenance is
   procedure Put_All;
private
   procedure Put_Header;
   procedure Put_High_Load_Hosts;
   procedure Put_Low_Load_Hosts;
   procedure Put_Swapping_Hosts;

   function High_Load (H : Hosts.Host) return Boolean;
   function Low_Load (H : Hosts.Host) return Boolean;
   function High_Swap (H : Hosts.Host) return Boolean;
end Maintenance;
