with Ahven.Framework;

package Gilded_Rose_Tests is
  type Test is new Ahven.Framework.Test_Case with null record;

  procedure Initialize(T : in out Test);

  private

  procedure Test_Gilded_Rose;
end Gilded_Rose_Tests;
