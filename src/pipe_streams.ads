with Input_Sources; use Input_Sources;
with Unicode;

with POSIX.IO; use POSIX.IO;
with POSIX.Process_Identification;
with POSIX; use POSIX;

package Pipe_Streams is
--  Stream read from a pipe, used to interface with xmlADA

   type Pipe_Stream is new Input_Source with private;

   Failed_Creation_Error : exception;
   Exception_Error : exception;
   Other_Error : exception;
   overriding procedure Next_Char (From : in out Pipe_Stream;
      C    : out Unicode.Unicode_Char);
--  Return a single character from From.
   overriding function Eof (From : Pipe_Stream) return Boolean;
   --  Return True if there is no more character to read on the stream
   overriding procedure Close (Input : in out Pipe_Stream);

   procedure Execute (P : in out Pipe_Stream;
                      Command : in String;
                      Arguments : String;
                      Environment : in String);
private
   type Pipe_Stream is new Input_Source with record
      Pipe        : File_Descriptor;
      Buffer      : IO_Buffer (1 .. 1_024);
      Position    : Natural := 0;
      Last_Read   : IO_Count := 0;
      --  points after the character last read
      Eof_Reached : Boolean := False;
      PID         : Process_Identification.Process_ID;
   end record;
end Pipe_Streams;
