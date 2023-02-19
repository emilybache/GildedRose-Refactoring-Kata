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

The test coverage can be found the files located in target/site/jacoco/*
To see the file, you must run *mvn clean test* before

    Element	Missed Instructions	Cov.	Missed Branches	Cov.	Missed	Cxty	Missed	Lines	Missed	Methods	Missed	Classes
    Total	18 of   287	                93%	    3 of 38	92%	    4  	    29	    1	    44	    1	    10	    0	    2
    com.gildedrose	18269	            93%	    335	    92%	    4	    29  	1	    44	    1	    10  	0	    2
