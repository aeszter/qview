with Input_Sources; use Input_Sources;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Unicode;
with Pipe_Commands;

package Pipe_Streams is
--  Stream read from a pipe, used to interface with xmlADA

   type Pipe_Stream is new Input_Source with private;
   overriding procedure Next_Char (From : in out Pipe_Stream;
      C    : out Unicode.Unicode_Char);
--  Return a single character from From.
   overriding function Eof (From : Pipe_Stream) return Boolean;
--  Return True if there is no more character to read on the stream

   procedure execute (p : in out Pipe_Stream;
                      Command : in String;
                      IO_type : in Pipe_Commands.IO_MODE);
   procedure Wait_For_Children;
private
   type Pipe_Stream is new Input_Source with record
      file_stream : Pipe_Commands.stream;
      line_buffer : Unbounded_String := Null_Unbounded_String;
      position    : Integer := 0;
      --  points after the character last read
      inbound_eof         : Boolean := False;
   end record;
end Pipe_Streams;
