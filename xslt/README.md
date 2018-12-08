# XSLT port of the Gilded-Rose Kata

This is a XSLT 1.0 port of the *Gilded-Rose-Kata*.

## Building and Running

* [Apache Ant's XSLT task](https://ant.apache.org/manual/Tasks/style.html) is used to run the transformations.
* `update_quality.xsl` contains the Gilded Rose logic.
* Run Ant in the current folder to transform all files.
* `texttest_fixture.xml` is transformed into `texttest_fixture.next_day.xml` with updated values.

## Unit Test

[xsltunit](http://xsltunit.org/) is a implementation of xUnit in XSLT.

* `tst_update_quality.xsl`is the the test (stylesheet).
* Run Ant in the current folder to run the tests.
* `update_quality.test_result.xml` contains the results and
  `update_quality.test_result.html` is a readable report.
