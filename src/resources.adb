package body Resources is

   ------------------
   -- New_Resource --
   ------------------

   function New_Resource (Name : Unbounded_String; Value : Unbounded_String)
                          return Resource is
      R : Resource;
   begin
      R.Name := Name;
      R.Value := Value;
      return R;
   end New_Resource;


end Resources;
