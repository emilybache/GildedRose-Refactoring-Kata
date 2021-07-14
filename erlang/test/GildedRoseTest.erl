-module('GildedRoseTest').

-include_lib("eunit/include/eunit.hrl").

-include("GildedRose.hrl").

update_quality_test_() -> [
  {"foo", ?_assertMatch([#item{name= "FixMe"}], 'GildedRose':update_quality([#item{name = "Foo", sell_in = 0, quality = 0}]))}
].
