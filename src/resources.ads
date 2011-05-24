with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;

package Resources is

   type Resource is record
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

   function New_Resource (Name  : String;
                          Value : Unbounded_String;
                          Boolean_Valued : Boolean;
                          State : Tri_State)
                          return Resource;
   function New_Resource (Name : String; Value : String)
                          return Resource;

   function Hash (R : Resource) return Hash_Type;



   package Resource_Lists is
     new Ada.Containers.Ordered_Maps (Element_Type => Resource,
                                      Key_Type     => Unbounded_String);

   procedure Put (Pos : Resource_Lists.Cursor);
   function Hash (List : Resource_Lists.Map) return String;
   function "<" (Left, Right : Resource) return Boolean;
   function Precedes (Left, Right : Resource_Lists.Map) return Boolean;

   function To_Unbounded_String (L : Resource_Lists.Map) return Unbounded_String;
   function To_String (L : Resource_Lists.Map) return String;


end Resources;
