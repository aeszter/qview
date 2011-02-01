with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Jobs; use Jobs;
with Resources;

package Bunches is

   type Bunch is record
      PE, Slots, Queue                          : Unbounded_String;
      State                                     : Job_State;
      Hard, Soft                                : Resources.Resource_Lists.List;
      Total_Slots, Slots_On_Hold, Slots_Waiting : Natural;

   end record;

   package Bunch_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Bunch);

   procedure Build_List (Job_List : in out Jobs.Job_Lists.List;
                         Bunch_List : out Bunch_Lists.List);
   function New_Bunch (J : Job) return Bunch;
end Bunches;
