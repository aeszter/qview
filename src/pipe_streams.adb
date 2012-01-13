with Unicode;
with Ada.Characters;
with Ada.Characters.Latin_1;
with POSIX.Process_Primitives; use POSIX.Process_Primitives;
with POSIX.Process_Identification; use POSIX.Process_Identification;
with POSIX; use POSIX;

package body Pipe_Streams is

   ---------------
   -- Next_Char --
   ---------------

   overriding procedure Next_Char
     (From : in out Pipe_Stream;
      C    : out Unicode.Unicode_Char)
   is
   begin
      if From.Position >= Length (From.Line_Buffer) then
         --  Buffer_Line (From);
         C := Unicode.To_Unicode (Ada.Characters.Latin_1.LF);
      else
         From.Position := From.Position + 1;
         C := Unicode.To_Unicode (Element (From.Line_Buffer, From.Position));
      end if;
   end Next_Char;

   ---------
   -- Eof --
   ---------

   overriding function Eof (From : Pipe_Stream) return Boolean is
   begin
      return From.Inbound_EOF;
      --  assume that the last line ends with a line feed
      --  otherwise, we would have to return true until the last character
      --  of From.line_buffer has been delivered
   end Eof;


   -------------
   -- execute --
   -------------

   procedure Execute
     (P : in out Pipe_Stream;
      Command : in String)
   is
      To_QView : POSIX.IO.File_Descriptor;
      Template : Process_Template;
      Arg_List : POSIX_String_List;
   begin
      POSIX.IO.Create_Pipe (Read_End  => P.Pipe,
                            Write_End => To_QView);
      Set_File_Action_To_Close (Template => Template,
                                File     => P.Pipe);
      Start_Process (Child    => P.PID,
                     Pathname => To_POSIX_String (Command),
                     Template => Template,
                     Arg_List => Arg_List);
   end Execute;

end Pipe_Streams;
