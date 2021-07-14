GildedRose
=====

You will need to install [erlang](https://www.erlang.org/), and [rebar3](https://github.com/erlang/rebar3). I recommend following the instructions from JetBrains: [Getting Started with Erlang](https://www.jetbrains.com/help/idea/getting-started-with-erlang.html).

When you open this project with IntelliJ I recommend that you do 

    File | New | Project from existing sources 

Then be sure to select "Erlang" as the project type, and configure the rebar3 location correctly.

Build
-----

    $ rebar3 compile

Run Tests
---------

    $ rebar3 eunit

TextTest Fixture
----------------
To run for 30 days:

    $ update_quality_main.escript 30