-module(texttest_fixture).

-include("texttest_fixture.hrl").

-export([print_update_quality/2]).

print_one_day(Day, Items) ->
  io:format("~n-------- day ~p --------~n", [Day]),
  io:format("name, sellIn, quality~n", []),
  lists:foreach(fun(#item{name = Name, sell_in = SellIn, quality = Quality}) ->
    io:format("~s, ~p, ~p~n", [Name, SellIn, Quality])
                end, Items).

print_update_quality(Days, Items) ->
  io:format("OMGHAI!"),
  lists:foreach(
    fun(Day) -> print_one_day(Day, Items),
      gilded_rose:update_quality(Items)
    end, lists:seq(0, Days - 1)
  ).

