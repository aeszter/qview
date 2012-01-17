with Unicode;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with POSIX; use POSIX;
with Ada.Strings.Fixed;
with POSIX.Process_Environment; use POSIX.Process_Environment;
with Ada.IO_Exceptions;

package body Pipe_Streams is

   procedure To_String_List
     (Source  : String;
      Dest    : out POSIX_String_List);

   ---------------
   -- Next_Char --
   ---------------

   overriding procedure Next_Char
     (From : in out Pipe_Stream;
      C    : out Unicode.Unicode_Char) is
   begin
      if From.Position >= Integer (From.Last_Read) then
         POSIX.IO.Read (File           => From.Pipe,
                        Buffer         => From.Buffer,
                        Last           => From.Last_Read);
         From.Position := 0;
      end if;
      From.Position := From.Position + 1;
      C := Unicode.To_Unicode (Standard.Character (From.Buffer (From.Position)));
   exception
      when Ada.IO_Exceptions.End_Error =>
         From.Eof_Reached := True;
         C := Unicode.To_Unicode (Standard.Character (LF));
      when others =>
         raise;
   end Next_Char;

   ---------
   -- Eof --
   ---------

   overriding function Eof (From : Pipe_Stream) return Boolean is
   begin
      return From.Eof_Reached;
   end Eof;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Input : in out Pipe_Stream) is
      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status => Status, Child => Input.PID);
      Input_Sources.Close (Input_Source (Input));
      case Exit_Status_Of (Status) is
         when Normal_Exit => return;
         when Failed_Creation_Exit => raise Failed_Creation_Error;
            when Unhandled_Exception_Exit => raise Exception_Error;
         when others => raise Other_Error with Exit_Status_Of (Status)'Img;
      end case;
   end Close;

   --------------------
   -- To_String_List --
   --------------------

   procedure To_String_List
     (Source  : String;
      Dest   : out POSIX_String_List)
   is
      Next_Index : Natural := 1;
      Index_List : array (1 .. 256) of Natural;
   begin
      Index_List (Next_Index) := Source'First;
      while Index_List (Next_Index) < Source'Last loop
         Next_Index := Next_Index + 1;
         Index_List (Next_Index) := 1 + Ada.Strings.Fixed.Index (Source (Index_List (Next_Index - 1) .. Source'Last), " ");
         if Index_List (Next_Index) = 1 then
            Index_List (Next_Index) := Source'Last + 2;
         end if;
         POSIX.Append (Dest, To_POSIX_String (Source (Index_List (Next_Index - 1) .. Index_List (Next_Index) - 2)));
      end loop;
   end To_String_List;


   -------------
   -- execute --
   -------------

   procedure Execute (P           : in out Pipe_Stream;
                      Command     : in String;
                      Arguments   : String;
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

      To_String_List (Source => Command & " " & Arguments,
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

end Pipe_Streams;
