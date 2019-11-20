with Slurm.Share_Tree;

package Share_Tree is
   procedure Put_All (Sort_Field : String; Sort_Direction : String);
   procedure Sort_By (Field : String; Direction : String);

private
   procedure Put (Item : Slurm.Share_Tree.Lists.Cursor);

end Share_Tree;
