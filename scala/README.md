# Gilded Rose starting position in Scala 3 scalatest


## Build the project

``` cmd
sbt compile
```

## Run the Gilded Rose Command-Line program

For e.g. 10 days:

``` cmd
sbt "run 10"
```

## Run all the unit tests

``` cmd
sbt test
```

## Run approval tests

The approval test can be found in the src/test/scala/GildedRoseApprovalTest.scala. It uses the approval test framework from https://github.com/approvals/ApprovalTests.Java.

The initial run will be a failing test and suggests an output to accept as the first approved version. Each subsequent run of the test will succeed until the output from the test changes.

To approve a new version, simply accept some or all diffs between the existing approved files and the newly generated output.

Approval tests are part of the test suite triggered by "sbt test". 

``` cmd
sbt test
```

## Run texttest version of approval tests

To run the texttest version, first edit the texttest configuration file. It contains to sections for scala that need to be uncommented.

* the executable and interpreter lines to run texttest_rig.py to trigger an sbt run
* the four lines in the run_dependent_text section to ignore sbt's info and success lines in the output.

Be aware that running the texttest version is somewhat slow since starting sbt takes some time. 