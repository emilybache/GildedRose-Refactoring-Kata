with Gilded_Rose;
use Gilded_Rose;

with Items;
use Items;

with Ahven;

with Ada.Strings.Unbounded;

package body Gilded_Rose_Tests is
  procedure Initialize(T : in out Test) is
  begin
    Set_Name(T, "Gilded_Rose_Test");
    Ahven.Framework.Add_Test_Routine(T, Test_Gilded_Rose'Access, "Foo");
  end Initialize;

  procedure Test_Gilded_Rose is
    Things : Item_Vecs.Vector;
  begin
    Things.Append(New_Item =>
      (Name => SU.To_Unbounded_String("Foo"),
      Sell_In => 0,
      Quality => 0));
    declare
      App : Gilded_Rose.Gilded_Rose := (Items => Things);

      package SU renames Ada.Strings.Unbounded;
      procedure Assert_Eq_Unbounded_String is
        new Ahven.Assert_Equal(Data_Type => SU.Unbounded_String, Image => SU.To_String);
    begin
      Update_Quality(App);
      Assert_Eq_Unbounded_String(Actual => App.Items(Item_Vecs.First(App.Items)).Name,
        Expected => SU.To_Unbounded_String("fixme"),
        Message => "fixme");
    end;
  end;
end Gilded_Rose_Tests;
