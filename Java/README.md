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

#### On Archlinux

run

    sudo gradle  -q text --args 30 > {Path}/texttests/ThirtyDays/stdout.gr

Where Path is the relative path to the texttests folder
At each run, the stdout.gr will be updated

### test cases:

There are two unit tests:
    * GildedRoseTest
    * ItemTest

Run
    mvn clean test

### test coverage:

100%
