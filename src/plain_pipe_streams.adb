with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Environment; use POSIX.Process_Environment;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Utils;

package body Plain_Pipe_Streams is

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out Plain_Pipe_Stream;
      C    : out Character)
   is
   begin
      if From.Position >= Integer (From.Last_Read) then
         POSIX.IO.Read (File           => From.Pipe,
                        Buffer         => From.Buffer,
                        Last           => From.Last_Read);
         From.Position := 0;
      end if;
      From.Position := From.Position + 1;
      C := Standard.Character (From.Buffer (From.Position));
   exception
      when Ada.IO_Exceptions.End_Error =>
         From.Eof_Reached := True;
         C := Standard.Character (LF);
      when others =>
         raise;
   end Next_Char;

   ---------
   -- Eof --
   ---------

   function Eof (From : Plain_Pipe_Stream) return Boolean is
   begin
      return From.Eof_Reached;
   end Eof;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out Plain_Pipe_Stream) is
      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status => Status, Child => Input.PID);
      case Exit_Status_Of (Status) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Failed_Creation_Error;
            when Unhandled_Exception_Exit => raise Exception_Error;
         when others => raise Other_Error with Exit_Status_Of (Status)'Img;
      end case;
   end Close;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (P : in out Plain_Pipe_Stream;
      Command : in String;
      Arguments : String;
      Environment : in String)
   is
      To_QView : POSIX.IO.File_Descriptor;
      Template : Process_Template;
      Arg_List : POSIX_String_List;
      Separator : Natural := Ada.Strings.Fixed.Index (Environment, "=");
      Env : POSIX.Process_Environment.Environment;
   begin
      POSIX.IO.Create_Pipe (Read_End  => P.Pipe,
                            Write_End => To_QView);
      Open_Template (Template);
      Set_File_Action_To_Close (Template => Template,
                                File     => P.Pipe);
      Set_File_Action_To_Duplicate (Template  => Template,
                                    File      => Standard_Output,
                                    From_File => To_QView);

      Utils.To_String_List (Source => Command & " " & Arguments,
                            Dest   => Arg_List);
      Set_Environment_Variable
        (Name  => To_POSIX_String (Environment (Environment'First .. Separator - 1)),
         Value => To_POSIX_String (Environment (Separator + 1 .. Environment'Last)),
         Env   => Env);

      Start_Process (Child    => P.PID,
                     Pathname => To_POSIX_String (Command),
                     Template => Template,
                     Arg_List => Arg_List,
                     Env_List => Env);
      Close (File => To_QView);
   end Execute;

end Plain_Pipe_Streams;
