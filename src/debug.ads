package Debug is
   type Facility is (Default);
   type Level is range 1 .. 10;
   procedure Enable (What : Facility);
   procedure Set_Level (Severity : Level);
   procedure Disable;
   procedure Log (Message : String; Where : Facility; Severity : Level);
   procedure Initialize (Input : String);
private
   Enabled : array (Facility) of Boolean := (Default => False);
   Debug_Level : Level;
end Debug;
