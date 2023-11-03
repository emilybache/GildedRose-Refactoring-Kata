# Gilded Rose starting position in Csharp Core

## Build the project

Use your normal build tools. 

## Run the TextTest fixture from the Command-Line

For e.g. 10 days:

```
GildedRoseTests/bin/Debug/net7.0/GildedRoseTests 10
```

You should make sure the command shown above works when you execute it in a terminal before trying to use TextTest (see below). If your tooling has placed the executable somewhere else, you will need to adjust the path above.


## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest. You will need to specify the GildedRoseTests executable and interpreter in [config.gr](../texttests/config.gr). Uncomment this line:

    executable:${TEXTTEST_HOME}/csharpcore/GildedRoseTests/bin/Debug/net7.0/GildedRoseTests

