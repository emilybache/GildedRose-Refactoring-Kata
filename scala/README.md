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

The approval test creates a file with an result of the test fixture in approvaltests/gildedrose.testresult.txt. The very first test run will fail and the resulting gildedrose.testresult.txt can be moved/renamed to gildedrose.approved.txt.

The approval tests in the test directory under GildedRoseApprovalTest.scala will regenerate the test result on each run and compare it to the approved file. The approval test will fail when thee are any differences between the files.

To approve a new version simply rename the resulting test output to gildedrose.approved.txt

Approval tests are part of the test suite triggered by sbt. 

``` cmd
sbt test
```

## Run texttest version of approval tests

To run the texttest version, first edit the texttest configuration file. It contains to sections for scala that need to be uncommented.

* the executable and interpreter lines to run texttest_rig.py to trigger an sbt run
* the four lines in the run_dependent_text section to ignore sbt's info and success lines in the output.

Be aware that running the texttest version is somewhat slow since starting sbt takes some time. 