with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Jobs; use Jobs;
with Resources;
with Ranges; use Ranges;

package Bunches is

   type Bunch is record
      PE, Queue      : Unbounded_String;
      Slot_Number    : Unbounded_String;
      Slot_List      : Ranges.Range_Lists.List;
      Hard, Soft     : Resources.Hashed_List;
      Total, On_Hold : Natural;
      Error, Waiting : Natural;

   end record;

   package Bunch_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Bunch);

   procedure Build_List;
   procedure Put (Pos : Bunch_Lists.Cursor);

   function New_Bunch (J : Job) return Bunch;
   ---------
   -- "=" --
   --  Returns True if a given Job does belong to a given Bunch
   ---------

   function "=" (Left : Bunch; Right : Job) return Boolean;
   function "=" (Left : Job; Right : Bunch) return Boolean;
end Bunches;
