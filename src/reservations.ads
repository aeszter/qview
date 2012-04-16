with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
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

   type Reservation is record
      Job_ID : Positive;
      Column2 : Natural;
      State   : Reservation_State;
      Timestamp : Positive;
      Duration  : Positive;
      Queue     : String (1 .. 9);
      Resource_Type : Unbounded_String;
      Resource_Value : Resource_Value_Type;
      Confirmation : Boolean := False; -- whether this confirms an earlier reservation
   end record;

   type Index_Card is record
      Schedule_Run : Natural;
      Queue : String (1 .. 9);
      Job_ID : Positive;
   end record;

   function Card_Hash (Card : Index_Card) return Ada.Containers.Hash_Type;
   function Equivalent_Cards (Left, Right : Index_Card) return Boolean;
   function Exists_At (What : Reservation; Iteration : Natural) return Boolean;

   package Lists is new Vectors (Element_Type => Reservation, Index_Type => Positive);
   package Catalogs is new Hashed_Maps (Key_Type => Index_Card,
                                        Element_Type => Positive,
                                        Hash         => CArd_Hash,
                                       Equivalent_Keys => Equivalent_Cards);

   procedure Put (Position : Lists.Cursor);
   procedure Read_Line (Data : out Reservation; Store_Data : out Boolean);

   List : Lists.Vector;
   Catalog : Catalogs.Map;
   Schedule_File : Ada.Text_IO.File_Type;
   Schedule_File_Name : constant String := "/cm/shared/apps/sge/current/"
     & "default/common/schedule";
   Iteration_Number : Natural := 1;

end Reservations;
