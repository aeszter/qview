with Ada.Exceptions; use Ada.Exceptions;
with HTML;
with Viewer;
with CGI;
use CGI;
--  with SGE.Actions;
--  with Lightsout;
--  with CM.Power; use CM.Power;
with Ada.Strings.Fixed;
with GNAT.Lock_Files;
--  with CM.Taint;
--  with CM.Debug;
--  with SGE.Utils; use SGE.Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;

package body Actions is

   procedure Invoke (What : String) is
   begin
      raise Permission_Error with "unimplemented";
   end Invoke;

--     procedure Change_Maintenance (Node, Bug : String; To : Lightsout.Maintenance);
--     procedure Force_Kill;
--     procedure Silence (Message : String);

--     procedure Change_Maintenance (Node, Bug : String; To : Lightsout.Maintenance) is
--        Separator : constant Natural := Ada.Strings.Fixed.Index (Node, ".");
--        Short_Name : constant String := Node (Node'First .. Separator - 1);
--     begin
--        if not User_Is_Operator (CGI.Get_Environment ("REMOTE_USER")) then
--           raise Permission_Error with "you must be registered as an operator";
--        end if;
--        Lightsout.Clear;
--        Lightsout.Lock;
--        Lightsout.Read;
--        if Separator = 0 then
--           Lightsout.Set_Maintenance (Node, Bug, To);
--        else
--           Lightsout.Set_Maintenance (Short_Name, Bug, To);
--        end if;
--        Lightsout.Write;
--        Lightsout.Unlock;
--     end Change_Maintenance;
--
--     procedure Force_Kill is
--        Job_List : Unbounded_String := Null_Unbounded_String;
--     begin
--        if not User_Is_Manager (CGI.Get_Environment ("REMOTE_USER")) then
--           raise Permission_Error with "you must be registered as a manager";
--        end if;
--        for Index in 1 .. Key_Count ("j") loop
--           Append (Job_List, To_String (Value ("j", Index)) & ",");
--        end loop;
--        if Key_Count ("j") = 0 then
--           raise Constraint_Error;
--        else
--           Trim (Job_List,
--                 Left  => Ada.Strings.Maps.Null_Set,
--                 Right => Ada.Strings.Maps.To_Set (','));
--        end if;
--        SGE.Actions.Force_Kill (To_String (Job_List));
--     end Force_Kill;
--
--     procedure Silence (Message : String) is
--     begin
--        null; -- no facility yet. Cannot print to http since headers have
--        --  not been sent, but maybe use a file in the future?
--     end Silence;
--
--
--     ------------
--     -- Invoke --
--     ------------
--
--     procedure Invoke (What : String) is
--        procedure Put_Result;
--        Referrer : constant String := CGI.Get_Environment ("HTTP_REFERER");
--
--        procedure Put_Result is
--        begin
--           if Referrer /= "" then
--              CGI.Put_CGI_Header ("Location: " & Referrer);
--           else
--              Viewer.Put_Result ("OK");
--           end if;
--        end Put_Result;
--
--     begin
--        CM.Debug.Initialize (Silence'Access);
--        if What = "cj" then
--           SGE.Actions.Clear_Error (The_Job => Integer'Value (HTML.Param ("j")));
--           Put_Result;
--        elsif What = "cq" then
--           SGE.Actions.Clear_Error (The_Node => HTML.Param ("q"));
--           Put_Result;
--        elsif What = "k" then
--           SGE.Actions.Kill_Job (The_Job => Integer'Value (HTML.Param ("j")));
--           Put_Result;
--        elsif What = "forcekill" then
--           Force_Kill;
--           Put_Result;
--        elsif What = "eq" then
--           SGE.Actions.Enable (The_Node => HTML.Param ("q"), Use_Sudo => True);
--           Put_Result;
--        elsif What = "clear_maint" then
--           Change_Maintenance (Node => HTML.Param ("h"),
--                               To   => Lightsout.none,
--                               Bug  => HTML.Param ("bug"));
--           Put_Result;
--        elsif What = "maint_ignore" then
--           Change_Maintenance (Node => HTML.Param ("h"),
--                               To   => Lightsout.ignore,
--                               Bug  => HTML.Param ("bug"));
--           Put_Result;
--        elsif What = "maint_disable" then
--           Change_Maintenance (Node => HTML.Param ("h"),
--                               To   => Lightsout.disable,
--                               Bug  => HTML.Param ("bug"));
--           Put_Result;
--        elsif  What = "maint_poweroff" then
--           Change_Maintenance (Node => HTML.Param ("h"),
--                               To   => Lightsout.off,
--                               Bug  => HTML.Param ("bug"));
--           Put_Result;
--        elsif What = "poweron" then
--           Poweron (What => CM.Taint.Sanitise (HTML.Param ("h")),
--                    Sudo_User => CM.Taint.Implicit_Trust (CGI.Get_Environment ("REMOTE_USER")));
--           --  the call to Implicit_Trust means we have to run in a controlled environment
--           --  (i.e. a web server). Calling Sanitise instead does not help since the main danger
--           --  does not lie in weird strings, but in faked, non-authenticated user names.
--           --  There is no way to check this inside the code, we have to trust
--           --  our environment.
--           Put_Result;
--        else
--           Viewer.Put_Error ("Unknown action """ & What & """");
--        end if;
--     exception
--        when E : Permission_Error =>
--           Viewer.Put_Error ("Insufficient permissions: " & Exception_Message (E));
--        when Constraint_Error =>
--           Viewer.Put_Error ("Internal error: " & HTML.Param ("j") & " is no valid job");
--        when E : SGE.Actions.Subcommand_Error =>
--           Viewer.Put_Error ("Backend error: " & Exception_Message (E));
--        when GNAT.Lock_Files.Lock_Error =>
--           Viewer.Put_Error ("Could not lock lightsout file. "
--                               & "Reload page to try again.");
--        when E : others =>
--           Viewer.Put_Error ("Unexpected error: " & Exception_Message (E));
--     end Invoke;

end Actions;
