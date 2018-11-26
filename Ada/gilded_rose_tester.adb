with Ahven.Text_Runner;
with Ahven.Framework;
with Gilded_Rose_Tests;

procedure Gilded_Rose_Tester is
  S : Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite("All");
begin
  Ahven.Framework.Add_Test(S, new Gilded_Rose_Tests.Test);
  Ahven.Text_Runner.Run(S);
end Gilded_Rose_Tester;
