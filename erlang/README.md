GildedRose
=====

You will need to install [erlang](https://www.erlang.org/), and [rebar3](https://github.com/erlang/rebar3). I recommend following the instructions from JetBrains: [Getting Started with Erlang](https://www.jetbrains.com/help/idea/erlang.html).

When you open this project with IntelliJ I recommend that you do 

    File | New | Project from existing sources 

Then be sure to select "Erlang" as the project type, and configure the rebar3 location correctly, 
(There is a copy of it in this repo).

If you're having trouble executing the 'main' application, check your module path in your Project Settings.
I found I had to manually set the module compile output path to '_build/default/lib/gilded_rose/ebin'. 
The test output path seemed to be correct as '.eunit'

Build
-----

    $ rebar3 compile

Run Tests
---------

    $ rebar3 eunit

TextTest Fixture
----------------
To run for 30 days, I think this ought to work but I tend to run it through the IDE instead:

    $ erl -pa _build/default/lib/gilded_rose/ebin -pa . -eval main:main([30]). -s init stop -noshell
