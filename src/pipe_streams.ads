with Input_Sources; use Input_Sources;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Unicode;

with POSIX.IO; use POSIX.IO;
with POSIX.Process_Identification;

package Pipe_Streams is
--  Stream read from a pipe, used to interface with xmlADA

   type Pipe_Stream is new Input_Source with private;
   overriding procedure Next_Char (From : in out Pipe_Stream;
      C    : out Unicode.Unicode_Char);
--  Return a single character from From.
   overriding function Eof (From : Pipe_Stream) return Boolean;
--  Return True if there is no more character to read on the stream

   procedure Execute (P : in out Pipe_Stream;
                      Command : in String);
private
   type Pipe_Stream is new Input_Source with record
      Pipe : POSIX.IO.File_Descriptor;
      Line_Buffer : Unbounded_String := Null_Unbounded_String;
      Position    : Integer := 0;
      --  points after the character last read
      Inbound_EOF         : Boolean := False;
      PID : POSIX.Process_Identification.Process_ID;
   end record;
end Pipe_Streams;
