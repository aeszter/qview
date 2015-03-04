with Ada.Exceptions; use Ada.Exceptions;
with HTML;
with Viewer;
with CGI;
with SGE.Actions;

package body Actions is

   ------------
   -- Invoke --
   ------------

   procedure Invoke (What : String) is
      procedure Put_Result;
      Referrer : constant String := CGI.Get_Environment ("HTTP_REFERER");

      procedure Put_Result is
      begin
         if Referrer /= "" then
            CGI.Put_CGI_Header ("Location: " & Referrer);
         else
            Viewer.Put_Result ("OK");
         end if;
      end Put_Result;

   begin
      if What = "cj" then
         SGE.Actions.Clear_Error (The_Job => Integer'Value (HTML.Param ("j")));
         Put_Result;
      elsif What = "cq" then
         SGE.Actions.Clear_Error (The_Node => HTML.Param ("q"));
         Put_Result;
      elsif What = "k" then
         SGE.Actions.Kill_Job (The_Job => Integer'Value (HTML.Param ("j")));
         Put_Result;
      elsif What = "eq" then
         SGE.Actions.Enable (The_Node => HTML.Param ("q"), Use_Sudo => True);
         Put_Result;
      else
         Viewer.Put_Error ("Unknown action """ & What & """");
      end if;
   exception
      when Constraint_Error =>
         Viewer.Put_Error ("Internal error: " & HTML.Param ("j") & " is no valid job");
      when E : SGE.Actions.Subcommand_Error =>
         Viewer.Put_Error ("Backend error: " & Exception_Message (E));
      when E : others =>
         Viewer.Put_Error ("Unexpected error: " & Exception_Message (E));
   end Invoke;

end Actions;
