package CSS is
   procedure Put;
private
   procedure Open_Block (Name : String);
   procedure Close_Block;

   Block_Is_Open : Boolean := False;
end CSS;
