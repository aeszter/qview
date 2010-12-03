with unicode;
with Ada.Characters;
with Ada.Characters.Latin_1;

package body Pipe_Streams is

   procedure Buffer_Line (p : in out Pipe_Stream) is
   begin
      p.line_buffer := Pipe_Commands.Read_Next (p.file_stream);
      p.position := 0;
   exception
      when Pipe_Commands.End_Of_File =>
         if p.inbound_eof then
            raise;
            --  caller has ignored or not tested eof, so we raise an exception
         end if;
         p.inbound_eof := True;
   end Buffer_Line;

   ---------------
   -- Next_Char --
   ---------------

   overriding procedure Next_Char
     (From : in out Pipe_Stream;
      C    : out Unicode.Unicode_Char)
   is
   begin
      if From.position >= Length (From.line_buffer) then
         Buffer_Line (From);
         C := Unicode.To_Unicode (Ada.Characters.Latin_1.LF);
      else
         From.position := From.position + 1;
         C := Unicode.To_Unicode (Element (From.line_buffer, From.position));
      end if;
   end Next_Char;

   ---------
   -- Eof --
   ---------

   overriding function Eof (From : Pipe_Stream) return Boolean is
   begin
      return From.inbound_eof;
      --  assume that the last line ends with a line feed
      --  otherwise, we would have to return true until the last character
      --  of From.line_buffer has been delivered
   end Eof;


   -------------
   -- execute --
   -------------

   procedure execute
     (p       : in out Pipe_Stream;
      Command : in String;
      IO_type : in Pipe_Commands.IO_Mode)
   is
   begin
      p.file_stream := Pipe_Commands.execute (Command => Command,
                                              IO_type => IO_Type);
      Buffer_Line (p);
   end execute;


end Pipe_Streams;
