with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with POSIX.Process_Environment;
with POSIX.Process_Identification;
with POSIX.User_Database;
with POSIX;
with CGI; use CGI;
with Slurm.Admin;
with Slurm.Errors;
with HTML;
with Viewer;

package body Actions is

   procedure Drop_Privileges (To : String);

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

--      procedure Silence (Message : String) is
--      begin
--         null; -- no facility yet. Cannot print to http since headers have
--         not been sent, but maybe use a file in the future?
--      end Silence;

   procedure Assert_No_Root is
      use POSIX.Process_Identification;
   begin
      if Get_Real_User_ID = Value ("0") then
         raise Program_Error with "This program should not be run as root "
           & "unless ""act"" is given";
      end if;
   end Assert_No_Root;

   procedure Drop_Privileges (To : String) is
      use POSIX;
      use POSIX.Process_Environment;
      use POSIX.Process_Identification;
      use POSIX.User_Database;

      User : User_Database_Item;
   begin
      User := Get_User_Database_Item (To_POSIX_String (To));

      Set_Group_ID (Group_ID_Of (User));
      Set_User_ID (User_ID_Of (User));
      Change_Working_Directory (Initial_Directory_Of (User));
   exception
      when E : POSIX_Error =>
         raise Permission_Error with "Unable to drop privileges: "
           & Exception_Message (E);
   end Drop_Privileges;

   procedure Invoke (What : String) is
      procedure Put_Result;
      Referrer : constant String := CGI.Get_Environment ("HTTP_REFERER");
      User : constant String := CGI.Get_Environment ("REMOTE_USER");

      procedure Put_Result is
      begin
         if Referrer /= "" then
            CGI.Put_CGI_Header ("Location: " & Referrer);
         else
            Viewer.Put_Result ("OK");
         end if;
      end Put_Result;

   begin
      if User = "" then
         raise Permission_Error with "Could not determine user authentication";
      else
         Drop_Privileges (To => User);
      end if;
      if What = "k" then
         Slurm.Admin.Kill_Job (ID => Integer'Value (HTML.Param ("j")));
         Put_Result;
      elsif What = "r" then
         Slurm.Admin.Release_Job (ID => Integer'Value (HTML.Param ("j")));
         Put_Result;
      else
         Viewer.Put_Error ("Unknown action """ & What & """");
      end if;
   exception
      when E : Permission_Error =>
         Viewer.Put_Error ("Insufficient permissions: " & Exception_Message (E));
      when Constraint_Error =>
         Viewer.Put_Error ("Internal error: " & HTML.Param ("j") & " is no valid job");
      when E : Slurm.Errors.Slurm_Error =>
         Viewer.Put_Error ("Slurm error: " & Exception_Message (E));
      when E : others =>
         Viewer.Put_Error ("Unexpected error: " & Exception_Message (E));
   end Invoke;

end Actions;
