with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Items is
  function Integer_Image(Self : in Integer) return String is
    Img : constant String := Integer'Image(Self);
  begin
    if Self < 0 then
      return Img;
    else
      return Img(2 .. Img'Length);
    end if;
  end;

  function To_String(Self : in Item) return String is
    Name    : constant Unbounded_String := Self.Name;
    Sell_In : constant Unbounded_String := To_Unbounded_String(Integer_Image(Self.Sell_In));
    Quality : constant Unbounded_String := To_Unbounded_String(Integer_Image(Self.Quality));

    Ret : Unbounded_String;
  begin
    Append(Ret, Name);
    Append(Ret, ", ");
    Append(Ret, Sell_In);
    Append(Ret, ", ");
    Append(Ret, Quality);

    return To_String(Ret);
  end;

end Items;
