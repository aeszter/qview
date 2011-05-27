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

   type Hashed_List is new Resource_Lists.Map with private;
   overriding procedure Insert
     (Container : in out Hashed_List;
      Key       : Unbounded_String;
      New_Item  : Resource;
      Position  : out Resource_Lists.Cursor;
      Inserted  : out Boolean);

   overriding procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   overriding procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   overriding procedure Include
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Put (Pos : Resource_Lists.Cursor);
   function Hash (List : Resource_Lists.Map) return String;
   function "<" (Left, Right : Resource) return Boolean;


   function Hash (List : Hashed_List) return String;
   function Precedes (Left, Right : Hashed_List) return Boolean;

   function To_Unbounded_String (L : Hashed_List) return Unbounded_String;
   function To_String (L : Hashed_List) return String;

   function Value (L : Hashed_List; Name : String) return String;
   function Numerical (L : Hashed_List; Name : String) return Integer;

private
   type Hashed_List is new Resource_Lists.Map with
      record
         Hashed : Boolean := False;
         Hash_Value   : Hash_Type;
      end record;



end Resources;
