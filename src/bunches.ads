with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Jobs; use Jobs;
with Resources;
with Slots; use Slots;

package Bunches is

   type Bunch is record
      PE, Queue : Unbounded_String;
      --  Slots : Unbounded_String;
      --  When/how/why is this needed? Probably just for running jobs,
      --  but bunches are only made up of waiting jobs, so we can ignore
      --  the "slots" tag
      Slot_List        : Slots.Slot_Lists.List;
      Hard, Soft       : Resources.Resource_Lists.Map;
      Total, On_Hold   : Natural;
      Error, Waiting   : Natural;

   end record;

   package Bunch_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Bunch);

   procedure Build_List (Job_List : in out Jobs.Job_Lists.List;
                         Bunch_List : out Bunch_Lists.List);
   procedure Put (Pos : Bunch_Lists.Cursor);

   function New_Bunch (J : Job) return Bunch;
end Bunches;
