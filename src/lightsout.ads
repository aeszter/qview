with Ada.Containers.Ordered_Maps;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;

package Lightsout is
   package Host_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 12);
   subtype Host_Name is Host_Names.Bounded_String;


   procedure Read;
   procedure Clear;
   function Get_Maintenance (From : String) return String;
   function Get_Bug (From : String) return String;

   Config_Error : exception;

private
   type Maintenance is (none, ignore, disable, off);

   type Host is record
      Maintain : Maintenance;
      Bug      : Natural := 0;
   end record;

   package Lists is new Ada.Containers.Ordered_Maps (Element_Type => Host,
                                                     Key_Type     => Host_Name,
                                                    "<" => Host_Names."<");

   List : Lists.Map;
   Bugzilla_URL : Ada.Strings.Unbounded.Unbounded_String;

end Lightsout;
