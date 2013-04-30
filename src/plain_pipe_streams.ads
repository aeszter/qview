with POSIX.IO; use POSIX.IO;
with POSIX.Process_Identification;
with POSIX; use POSIX;

package Plain_Pipe_Streams is
--  Stream read from a pipe, inspired by xmlADA,
--  but for plain latin_1 characters without any prolog

   type Plain_Pipe_Stream is tagged private;

   Failed_Creation_Error : exception;
   Exception_Error : exception;
   Other_Error : exception;
   procedure Next_Char (From : in out Plain_Pipe_Stream;
      C    : out Character);
--  Return a single character from From.
   function Eof (From : Plain_Pipe_Stream) return Boolean;
   --  Return True if there is no more character to read on the stream
   procedure Close (Input : in out Plain_Pipe_Stream);

   procedure Execute (P : in out Plain_Pipe_Stream;
                      Command : in String;
                      Arguments : String;
                      Environment : in String);
private
   type Plain_Pipe_Stream is tagged record
      Pipe        : File_Descriptor;
      Buffer      : IO_Buffer (1 .. 1_024);
      Position    : Natural := 0;
      Last_Read   : IO_Count := 0;
      --  points after the character last read
      Eof_Reached : Boolean := False;
      PID         : Process_Identification.Process_ID;
   end record;
end Plain_Pipe_Streams;
