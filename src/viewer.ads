with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Core;

package Viewer is
   sgeroot : constant String := "/cm/shared/apps/sge/current";
   procedure View;
   procedure Set_Params (Params : String);
   function Setup_Parser (Command  : String := "qstat";
                          Selector : String) return DOM.Core.Document;
private
   My_Params : Unbounded_String;
end Viewer;
