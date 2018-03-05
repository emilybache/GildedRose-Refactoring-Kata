-module(gilded_rose_tests).

-include("gilded_rose.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ITEMS, [
  #item{name = "+5 Dexterity Vest", sell_in = 10, quality = 20},
  #item{name = "Aged Brie", sell_in = 2, quality = 0},
  #item{name = "Elixir of the Mongoose", sell_in = 5, quality = 7},
  #item{name = "Sulfuras, Hand of Ragnaros", sell_in = 0, quality = 80},
  #item{name = "Sulfuras, Hand of Ragnaros", sell_in = -1, quality = 80},
  #item{name = "Backstage passes to a TAFKAL80ETC concert", sell_in = 15, quality = 20},
  #item{name = "Backstage passes to a TAFKAL80ETC concert", sell_in = 10, quality = 49},
  #item{name = "Backstage passes to a TAFKAL80ETC concert", sell_in = 5, quality = 49},
  #item{name = "Conjured Mana Cake", sell_in = 3, quality = 6}
]).

golden_master_test() ->
  Days = 30,
  {ok, IoDevice} = file:open("actual_output.txt", [write]),
  try
    lists:foldl(fun(Day, Items) ->
      io:format(IoDevice, "-------- day ~p --------~n", [Day]),
      io:format(IoDevice, "name, sellIn, quality~n", []),
      lists:foreach(fun(#item{name = Name, sell_in = SellIn, quality = Quality}) ->
        io:format(IoDevice, "~s, ~p, ~p~n", [Name, SellIn, Quality])
      end, Items),
      io:nl(IoDevice),
      gilded_rose:update_quality(Items)
    end, ?ITEMS, lists:seq(0, Days - 1))
  after
    file:close(IoDevice)
  end,
  case file:read_file("expected_output.txt") of
    {ok, ExpectedFile} ->
      {ok, ActualFile} = file:read_file("actual_output.txt"),
      ?assertEqual(ExpectedFile, ActualFile);
    {error, Reason} ->
      ?debugFmt("Could not read file 'expected_output.txt': ~p", [Reason])
  end.

update_quality_test_() -> [
  {"first test", ?_assertMatch([#item{}], gilded_rose:update_quality([#item{}]))}
].
