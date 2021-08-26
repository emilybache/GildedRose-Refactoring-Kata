-module(main).

-include("texttest_fixture.hrl").

-export([main/1]).

-define(ALL_ITEMS, [
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

main([Days]) ->
  try
    texttest_fixture:print_update_quality(Days, ?ALL_ITEMS)
  catch
    ExceptionClass:ExceptionInfo ->
      io:format("something went wrong:~n~p : ~p~n", [ExceptionClass, ExceptionInfo]),
      usage()
  end;

main(_) ->
  usage().

usage() ->
  io:format("usage: main main number_of_days\ne.g. main main [30]\n"),
  halt(1).

