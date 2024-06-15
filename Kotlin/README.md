# Gilded Rose starting position in Kotlin

## Run the Text Fixture from Command-Line

```
./gradlew -q text
```

### Specify Number of Days

For e.g. 10 days:

```
./gradlew run --args 10
```

You should make sure the gradle commands shown above work when you execute them in a terminal before trying to use TextTest (see below).


## Run the TextTest approval test that comes with this project

There are instructions in the [TextTest Readme](../texttests/README.md) for setting up TextTest. What's unusual for the Java version is there are two executables listed in [config.gr](../texttests/config.gr) for Java. One uses Gradle wrapped in a python script, the other relies on your CLASSPATH being set correctly in [environment.gr](../texttests/environment.gr).


### Project info
1. Code is highly unreadable and impossible to extend
2. Algorithm and test cases are unknown, I do not trust that `TexttestFixtures` covers all paths so I want to validate it with jacoco
3. Codebase is just 60 LOC and edge-values seems to be small values 

### Plan of action
1. Generate all test cases / convert `TexttestFixtures` to tests
2. Break complex statements, make code longer and simpler
3. understand algorithm and propose better solution


### Execution
TexttestFixtures indeed do not cover all paths, based on jacoco coverage: 
 ![img.png](img.png)

There are two ways to advance:
1. manually craft extra test cases
2. auto generate a bunch of test cases
 
I prefer to go with (2) because it seems to me to be more resilient way. I need to check if execution 
time is not bloated. Also minimize number of test cases.
