
package Actions is
   procedure Invoke (What : String)
     with Pre => (What /= "");

   procedure Assert_No_Root;

   Permission_Error : exception;
end Actions;
