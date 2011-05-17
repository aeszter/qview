with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Core;

package Viewer is
   sgeroot : constant String := "/cm/shared/apps/sge/current";
   Resource_Selector : constant String := "-F h_rt,eth,ib,mem_total,num_proc,cm";
   procedure View;
   procedure Set_Params (Params : String);
   procedure Append_Params (Params : String);
   --  Purpose: Add a parameter to the list kept to create links that preserve
   --          user selections
   --  Parameter Params: the parameter to add; should be of the form "param=value"
   function Setup_Parser (Command  : String := "qstat";
                          Selector : String) return DOM.Core.Document;
private
   My_Params : Unbounded_String;
end Viewer;
