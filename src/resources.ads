with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils;
with Ada.Containers; use Ada.Containers;

package Resources is

   type Resource is record
      Name           : Unbounded_String;
      Value          : Unbounded_String;
      Numerical      : Integer;
      Boolean_Valued : Boolean;
      State          : Tri_State;
   end record;

   type Network is (none, eth, ib);
   type CPU_Model is (none, italy, woodcrest, clovertown, harpertown, magnycours);

   Resource_Error : exception;

   function To_Model (S : Unbounded_String) return CPU_Model;
   function To_Model (S : String) return CPU_Model;
   function To_String (Model : CPU_Model) return String;
   function To_Network (S : String) return Network;
   function Format_Duration (Secs : Natural) return String;

   function New_Resource (Name  : Unbounded_String;
                          Value : Unbounded_String;
                          Boolean_Valued : Boolean;
                          State : Tri_State)
     return Resource;
   function New_Resource (Name : String; Value : String)
     return Resource;
   procedure Put (R : Resource);
   function Hash (R : Resource) return Hash_Type;



   package Resource_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Resource);

   procedure Sort (L : in out Resource_Lists.List);
   function Equal (Left, Right : Resource_Lists.List) return Boolean;
   function Hash (List : Resource_Lists.List) return String;
   function Precedes (Left, Right : Resource) return Boolean;
   function "<" (Left, Right : Resource) return Boolean;
   function Precedes (Left, Right : Resource_Lists.List) return Boolean;

   function Get_Value (List : Resource_Lists.List; Name : Unbounded_String)
         return Unbounded_String;
   function Get_Value (List : Resource_Lists.List; Name : String)
         return Unbounded_String;
   function Get_Numerical (List : Resource_Lists.List; Name : String)
         return Integer;
   function Get_Numerical (List : Resource_Lists.List; Name : Unbounded_String)
         return Integer;

   function To_Unbounded_String (L : Resource_Lists.List) return Unbounded_String;
   function To_String (L : Resource_Lists.List) return String;

   package Sorting is
      new Resource_Lists.Generic_Sorting ("<" => Precedes);

end Resources;
