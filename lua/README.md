# Gilded Rose starting position in Lua

I assume you have installed [Lua](https://lua.org/start.html) on your system (version >= 5.1).

## Install

If you want to use [LuaRocks](https://luarocks.org/) there is a config file that should install the dependencies nicely:

```sh
$ luarocks install --only-deps gildedrose-dev-1.rockspec
```

## Run unit tests from the command line

The tests use the [Busted](https://github.com/lunarmodules/busted) tool.

```sh
$ busted
```

For coverage you'll need to install [LuaCov](https://github.com/lunarmodules/luacov)

```sh
$ busted --coverage
$ luacov # generate the report
```

## Run the TextTest fixture on the command line

```sh
$ lua src/main.lua 10
```

## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest.
You will need to specify the executable in [config.gr](../texttests/config.gr).
Uncomment this line to use it:

```
executable:${TEXTTEST_HOME}/lua/texttest
```

The TextTest fixture use the `./texttest` executable as a hack to make the `require` work.
Lua does not have relative `require` and I did not find a simple way to implement them so the test need to run from the `./lua` directory.
I will improve this in the future if I learn how to do it better :)
