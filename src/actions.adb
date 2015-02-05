with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;
with HTML;
with Viewer;
with CGI;
with SGE.Actions;

package body Actions is

   ------------
   -- Invoke --
   ------------

   procedure Invoke (What : String) is
      Referrer : constant String := CGI.Get_Environment ("HTTP_REFERER");
   begin
      if What = "cj" then
         SGE.Actions.Clear_Error (The_Job => Integer'Value (HTML.Param ("j")));
         if Referrer /= "" then
            Ada.Text_IO.Put_Line ("HTTP/1.1 301 Moved Permanently");
            Ada.Text_IO.Put_Line ("Location: " & Referrer);
         else
            Viewer.Put_Result ("OK");
         end if;
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
