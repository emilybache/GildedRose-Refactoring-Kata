with Ada.Strings.Unbounded;

package Items is
  package SU renames Ada.Strings.Unbounded;

  type Item is record
    Name    : SU.Unbounded_String;
    Sell_In : Integer;
    Quality : Integer;
  end record;

  function To_String(Self : in Item) return String;

end Items;
