with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with POSIX.C; use POSIX.C;
with System;
with POSIX;
use POSIX;

package body Utils is

   function readlink (path : char_ptr; buf : System.Address; bufsize : size_t)
     return ssize_t;
   pragma Import (C, readlink, "readlink");

   procedure Read_Link (Path : String; Buffer : out String; Last : out Natural) is
      Pathname_With_NUL : constant POSIX_String := To_POSIX_String (Path) & POSIX.NUL;
      Result : ssize_t;
   begin
      Result := readlink (Pathname_With_NUL (Pathname_With_NUL'First)'Unchecked_Access,
                          Buffer (Buffer'First)'Address,
                          size_t (Buffer'Last - Buffer'First + 1));
      if Result = -1 then
         raise POSIX.POSIX_Error;
      end if;
      Last := Buffer'First + Integer (Result) - 1;
   end Read_Link;

   function To_String (Source : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (Source'Img, Left);
   end To_String;

end Utils;
