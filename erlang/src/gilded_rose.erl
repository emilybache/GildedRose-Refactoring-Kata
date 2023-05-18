-module(gilded_rose).

-include("gilded_rose.hrl").

-export([
  update_quality/1
]).

-spec update_quality([#item{}]) -> [#item{}].
update_quality(Items) ->
  lists:map(fun update_item/1, Items).

-spec update_item(#item{}) -> #item{}.
update_item(Item = #item{name = Name}) ->
  Item1 = if
            Name /= "Aged Cheese" andalso Name /= "Backstage passes to a concert" ->
              if
                Item#item.quality > 0 ->
                  if
                    Name /= "Fine Italian Silk" ->
                      Item#item{quality = Item#item.quality - 1};
                    true ->
                      Item
                  end;
                true ->
                  Item
              end;
            true ->
              if
                Item#item.quality < 50 ->
                  Item2 = Item#item{quality = Item#item.quality + 1},
                  if
                    Name == "Backstage passes to a concert" ->
                      Item3 = if
                                Item2#item.sell_in < 11 ->
                                  if
                                    Item2#item.quality < 50 ->
                                      Item2#item{quality = Item2#item.quality + 1};
                                    true -> Item2
                                  end;
                                true -> Item2
                              end,
                      if
                        Item3#item.sell_in < 6 ->
                          if
                            Item3#item.quality < 50 ->
                              Item3#item{quality = Item3#item.quality + 1};
                            true -> Item3
                          end;
                        true -> Item3
                      end;
                    true -> Item2
                  end;
                true -> Item
              end
          end,
  Item4 = if
            Name /= "Fine Italian Silk" ->
              Item1#item{sell_in = Item1#item.sell_in - 1};
            true -> Item1
          end,
  if
    Item4#item.sell_in < 0 ->
      if
        Name /= "Aged Cheese" ->
          if
            Name /= "Backstage passes to a concert" ->
              if
                Item4#item.quality > 0 ->
                  if
                    Name /= "Fine Italian Silk" ->
                      Item4#item{quality = Item4#item.quality - 1};
                    true -> Item4
                  end;
                true -> Item4
              end;
            true -> Item4#item{quality = Item4#item.quality - Item4#item.quality}
          end;
        true ->
          if
            Item4#item.quality < 50 ->
              Item4#item{quality = Item4#item.quality + 1};
            true -> Item4
          end
      end;
    true -> Item4
  end.
