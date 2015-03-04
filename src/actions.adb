with Ada.Exceptions; use Ada.Exceptions;
with HTML;
with Viewer;
with CGI;
with SGE.Actions;
with Lightsout;
with Ada.Strings.Fixed;
with GNAT.Lock_Files;

package body Actions is

   procedure Change_Maintenance (Node, Bug : String; To : Lightsout.Maintenance);

   procedure Change_Maintenance (Node, Bug : String; To : Lightsout.Maintenance) is
      Separator : constant Natural := Ada.Strings.Fixed.Index (Node, ".");
      Short_Name : constant String := Node (Node'First .. Separator - 1);
   begin
      --  when and how to lock?
      Lightsout.Clear;
      Lightsout.Lock;
      Lightsout.Read;
      Lightsout.Set_Maintenance (Short_Name, Bug, To);
      Lightsout.Write;
      Lightsout.Unlock;
   end Change_Maintenance;

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
      elsif What = "clear_maint" then
         Change_Maintenance (Node => HTML.Param ("h"),
                             To   => Lightsout.none,
                             Bug  => HTML.Param ("bug"));
         Put_Result;
      elsif What = "maint_ignore" then
         Change_Maintenance (Node => HTML.Param ("h"),
                             To   => Lightsout.ignore,
                             Bug  => HTML.Param ("bug"));
         Put_Result;
      elsif What = "maint_disable" then
         Change_Maintenance (Node => HTML.Param ("h"),
                             To   => Lightsout.disable,
                             Bug  => HTML.Param ("bug"));
         Put_Result;
      elsif  What = "maint_poweroff" then
         Change_Maintenance (Node => HTML.Param ("h"),
                             To   => Lightsout.off,
                             Bug  => HTML.Param ("bug"));
         Put_Result;
      else
         Viewer.Put_Error ("Unknown action """ & What & """");
      end if;
   exception
      when Constraint_Error =>
         Viewer.Put_Error ("Internal error: " & HTML.Param ("j") & " is no valid job");
      when E : SGE.Actions.Subcommand_Error =>
         Viewer.Put_Error ("Backend error: " & Exception_Message (E));
      when GNAT.Lock_Files.Lock_Error =>
         Viewer.Put_Error ("Could not lock lightsout file. "
                             & "Reload page to try again.");
      when E : others =>
         Viewer.Put_Error ("Unexpected error: " & Exception_Message (E));
   end Invoke;

end Actions;
