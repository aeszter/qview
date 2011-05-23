with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Viewer is
   procedure View;
   procedure Set_Params (Params : String);
   procedure Append_Params (Params : String);
   --  Purpose: Add a parameter to the list kept to create links that preserve
   --          user selections
   --  Parameter Params: the parameter to add; should be of the form "param=value"
private
   My_Params : Unbounded_String;
end Viewer;
