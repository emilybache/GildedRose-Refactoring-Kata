with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Gilded_Rose is
  procedure Update_Quality(Self : in out Gilded_Rose) is
    Cursor : Item_Vecs.Cursor := Item_Vecs.First(Self.Items);
  begin
    while Item_Vecs.Has_Element(Cursor) loop
      if Self.Items(Cursor).Name /= To_Unbounded_String("Aged Brie")
        and Self.Items(Cursor).Name /= To_Unbounded_String("Backstage passes to a TAFKAL80ETC concert") then
        if Self.Items(Cursor).Quality > 0 then
          if Self.Items(Cursor).Name /= To_Unbounded_String("Sulfuras, Hand of Ragnaros") then
            Self.Items(Cursor).Quality := Self.Items(Cursor).Quality - 1;
          end if;
        end if;
      else
        if Self.Items(Cursor).Quality < 50 then
          Self.Items(Cursor).Quality := Self.Items(Cursor).Quality + 1;

          if Self.Items(Cursor).Name = To_Unbounded_String("Backstage passes to a TAFKAL80ETC concert") then
            if Self.Items(Cursor).Sell_In < 11 then
              if Self.Items(Cursor).Quality < 50 then
                Self.Items(Cursor).Quality := Self.Items(Cursor).Quality + 1;
              end if;
            end if;

            if Self.Items(Cursor).Sell_In < 6 then
              if Self.Items(Cursor).Quality < 50 then
                Self.Items(Cursor).Quality := Self.Items(Cursor).Quality + 1;
              end if;
            end if;
          end if;
        end if;
      end if;

      if Self.Items(Cursor).Name /= To_Unbounded_String("Sulfuras, Hand of Ragnaros") then
        Self.Items(Cursor).Sell_In := Self.Items(Cursor).Sell_In - 1;
      end if;

      if Self.Items(Cursor).Sell_In < 0 then
        if Self.Items(Cursor).Name /= To_Unbounded_String("Aged Brie") then
          if Self.Items(Cursor).Name /= To_Unbounded_String("Backstage passes to a TAFKAL80ETC concert") then
            if Self.Items(Cursor).Quality > 0 then
              if Self.Items(Cursor).Name /= To_Unbounded_String("Sulfuras, Hand of Ragnaros") then
                Self.Items(Cursor).Quality := Self.Items(Cursor).Quality - 1;
              end if;
            end if;
          else
            Self.Items(Cursor).Quality := Self.Items(Cursor).Quality - Self.Items(Cursor).Quality;
          end if;
        else
          if Self.Items(Cursor).Quality < 50 then
            Self.Items(Cursor).Quality := Self.Items(Cursor).Quality + 1;
          end if;
        end if;
      end if;

      Item_Vecs.Next(Cursor);
    end loop;
  end;

end Gilded_Rose;
