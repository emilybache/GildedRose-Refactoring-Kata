# Gilded Rose starting position in Zig

I assume you have [installed](https://ziglang.org/learn/getting-started/#installing-zig) `zig` on your system.

## Run unit tests from the command line

```sh
$ zig build test
```

## Run the TextTest fixture on the command line

Build and install the executable

```sh
$ zig build
```

Execute it on the command line with an argument for the number of days:

```sh
$ ./zig/zig-out/bin/zig 10
```

## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest.
You will need to specify the executable in [config.gr](../texttests/config.gr).
Uncomment this line to use it:

```
#executable:${TEXTTEST_HOME}/zig/zig-out/bin/zig
```

