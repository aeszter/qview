with Ada.Containers.Ordered_Maps;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
with DOM.Core;

package Lightsout is
   package Host_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 12);
   subtype Host_Name is Host_Names.Bounded_String;
   type Maintenance is (none, ignore, disable, off);


   procedure Read;
   procedure Write;
   procedure Clear;
   function Get_Maintenance (From : String) return String;
   procedure Set_Maintenance (Node_Name, Bug : String; To : Maintenance);
   function Get_Bug (From : String) return String;
   function Get_Bug_ID (From : String) return String;
   function Get_Bug_ID (From : String) return Natural;
   function To_String (Source : Maintenance) return String;

   Config_Error : exception;

private

   type Host is record
      Maintain : Maintenance;
      Bug      : Natural := 0;
   end record;

   package Lists is new Ada.Containers.Ordered_Maps (Element_Type => Host,
                                                     Key_Type     => Host_Name,
                                                    "<" => Host_Names."<");

   List         : Lists.Map;
   Bugzilla_URL : Ada.Strings.Unbounded.Unbounded_String;
   XML_Doc      : DOM.Core.Document;


end Lightsout;
