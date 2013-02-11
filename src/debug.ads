package Debug is
   type Facility is (Default, Queues, Trace);
   type Level is range 1 .. 10;
   procedure Enable (What : Facility);
   procedure Set_Level (Severity : Level);
   procedure Disable;
   procedure Log (Message : String; Where : Facility; Severity : Level);
   procedure Trace (Entering : String; Params : String);
   procedure Initialize (Input : String);
private
   Enabled : array (Facility) of Boolean := (Default => False, Queues => False, Trace => False);
   Debug_Level : Level;
end Debug;
