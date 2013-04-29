with Spread_Sheets; use Spread_Sheets;

package Share_Tree is
   procedure Put_List;
   procedure Put_Summary;
   procedure Append_List (Cells : Spread_Sheet);
   procedure Sort_By (Field : String; Direction : String);
end Share_Tree;
