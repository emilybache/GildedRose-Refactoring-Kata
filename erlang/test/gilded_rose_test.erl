-module(gilded_rose_test).

-include_lib("eunit/include/eunit.hrl").

-include("gilded_rose.hrl").

update_quality_test_() -> [
  {"foo", ?_assertMatch([#item{name= "Foo"}], gilded_rose:update_quality([#item{name = "Foo", sell_in = 0, quality = 0}]))}
].
