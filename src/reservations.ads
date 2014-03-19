with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
use Ada.Containers;

package Reservations is
   procedure Put_All;
   procedure Read;

   Read_Error : exception;
   Buffer_Overrun : exception;
   Improper_Line : exception;
private

   type Reservation_State is (reserving, running, starting);
   type Resource_Value_Type is digits 7;

   type Queue is record
      Name : String (1 .. 9);
      Slots : Natural;
   end record;

   function To_String (Source : Queue) return String;
   function Precedes (Left, Right : Queue) return Boolean;

   package Queue_Lists is new Ordered_Sets (Element_Type => Queue, "<" => Precedes);
   subtype Queue_List is Queue_Lists.Set;

   function To_String (Source : Queue_List; Start : Queue_Lists.Cursor) return String;
   function To_String (Source : Queue_List) return String;

   type Reservation is record
      Job_ID : Positive;
      Iteration : Natural;
      Column2 : Natural;
      State   : Reservation_State;
      Timestamp : Positive;
      Duration  : Positive;
      Queue     : Queue_List;
--      Resource_Type : Unbounded_String;
--      Resource_Value : Resource_Value_Type;
--  ignored for now, see comment in body of procedure Read_Line
      Confirmation   : Boolean := False; -- whether this confirms an earlier reservation
      Shifted        : Boolean := False; -- whether an earlier reservation with different details exists
      Hidden         : Boolean := False; -- whether Put should ignore this reservation
   end record;

   type Index_Card is record
      Schedule_Run : Natural;
      Job_ID : Positive;
   end record;

   function Card_Hash (Card : Index_Card) return Ada.Containers.Hash_Type;
   function Equivalent_Cards (Left, Right : Index_Card) return Boolean;
   function Equivalent_Reservations (Left, Right : Reservation) return Boolean;

   package Lists is new Vectors (Element_Type => Reservation, Index_Type => Positive);
   package Catalogs is new Hashed_Maps (Key_Type => Index_Card,
                                        Element_Type => Positive,
                                        Hash         => Card_Hash,
                                        Equivalent_Keys => Equivalent_Cards);
   package Job_Pool is new Ordered_Sets (Element_Type => Positive);

   function Equivalent_At (What : Reservation; Iteration : Natural) return Lists.Cursor;

   procedure Put (Position : Lists.Cursor);
   procedure Read_Line (Data : out Reservation; Store_Data : out Boolean);

   List : Lists.Vector;
   Catalog : Catalogs.Map;
   Reserving_Jobs : Job_Pool.Set;
   Schedule_File : Ada.Text_IO.File_Type;
   Schedule_File_Name : constant String := "/cm/shared/apps/sge/current/"
     & "default/common/schedule";
      pragma Compile_Time_Warning (True, "hardcoded config");

   Iteration_Number : Natural := 1;

end Reservations;
