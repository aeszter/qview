with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Jobs; use Jobs;
with Resources;
with Ranges; use Ranges;

package Bunches is

   type Bunch is private;

   procedure Build_List;
   procedure Put_List;

   function New_Bunch (J : Job) return Bunch;

   function "=" (Left : Bunch; Right : Job) return Boolean;
   function "=" (Left : Job; Right : Bunch) return Boolean;
   --  return True if a given Job belongs to a certain Bunch

private
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

   procedure Put (Pos : Bunch_Lists.Cursor);

   List : Bunch_Lists.List;
end Bunches;
