package Debug is
type Facility is (Default);
type Level is range 1 .. 10;
procedure Enable (What : Facility);
procedure Set_Level (Severity : Level);
procedure Disable;
procedure Log (Message : String; Where : Facility; Severity : Level);
end Debug;
