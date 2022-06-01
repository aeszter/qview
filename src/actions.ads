
package Actions is
   procedure Invoke (What : String)
     with Pre => (What /= "");

   procedure Assert_No_Root;
   procedure Drop_Privileges;

   Permission_Error : exception;
end Actions;
