with HTML;
with Ada.Exceptions; use Ada.Exceptions;
with CGI;

package body Debug is

   procedure Initialize (Input : String) is
   begin
      if Input /= "" then
         Set_Level (Level'Value (Input));
         Enable (Default);
         Enable (Queues);
         Enable (Trace); -- should be set via a separate variable
         --  but for now, debug levels are sufficiently fine-grained
      end if;
   exception
      when Constraint_Error =>
         Disable;
      when E : others =>
         CGI.Put_CGI_Header;
               HTML.Error (Exception_Message (E) & "(""" & Input & """)");
   end Initialize;

   ------------
   -- Enable --
   ------------

   procedure Enable (What : Facility) is
   begin
      Enabled (What) := True;
   end Enable;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (Severity : Level) is
   begin
      Debug_Level := Severity;
   end Set_Level;

   -------------
   -- Disable --
   -------------

   procedure Disable is
   begin
      for What in Enabled'Range loop
         Enabled (What) := False;
      end loop;
   end Disable;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String; Where : Facility; Severity : Level) is
   begin
      if not Enabled (Where) or else Severity > Debug_Level then
         return;
      end if;
      HTML.Comment (Where'Img & ": " & Message);
   end Log;

   procedure Trace (Entering : String; Params : String) is
   begin
      if not Enabled (Trace) then
         return;
      end if;
      Log (Message  => "->" & Entering & " with " & Params,
           Where    => Trace,
           Severity => 5);
   end Trace;

end Debug;
