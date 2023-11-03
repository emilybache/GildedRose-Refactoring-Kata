# Gilded Rose starting position in Java

## Run the Text Fixture from Command-Line

```
./gradlew -q text
```

### Specify Number of Days

For e.g. 10 days:

```
./gradlew -q text --args 10
```

## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](https://github.com/emilybache/GildedRose-Refactoring-Kata/blob/main/texttests/README.md) for setting up TextTest. What's unusual for the Java version is there are two executables listed in [config.gr](https://github.com/emilybache/GildedRose-Refactoring-Kata/blob/main/texttests/config.gr) for Java. One uses Gradle wrapped in a python script, the other relies on your CLASSPATH being set correctly in [environment.gr](https://github.com/emilybache/GildedRose-Refactoring-Kata/blob/main/texttests/environment.gr).
