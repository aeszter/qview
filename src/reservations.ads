with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

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
      Queue     : Unbounded_String;
      Resource_Type : Unbounded_String;
      Resource_Value : Resource_Value_Type;
      Confirmation : Boolean := False; -- whether this confirms an earlier reservation
   end record;

   package Lists is new Doubly_Linked_Lists (Element_Type => Reservation);

   procedure Put (Position : Lists.Cursor);
   procedure Read_Line (Data : out Reservation; Store_Data : out Boolean);

   List : Lists.List;
   Schedule_File : Ada.Text_IO.File_Type;
   Schedule_File_Name : constant String := "/cm/shared/apps/sge/current/"
     & "default/common/schedule";
   Iteration_Number : Natural := 0;

end Reservations;
