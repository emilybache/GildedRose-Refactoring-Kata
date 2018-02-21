# Pascal port of the Gilded-Rose Kata

This is a (Turbo) Pascal port of the *Gilded-Rose-Kata*.

## Building and Running

* Compile the unit `ROSE.PAS`, this is the Gilded Rose Logic.
* Compile the application `TEXTTEST.PAS` for the Texttest fixture.
* Run `TEXTTEST`.

## Unit Test

`TPUNIT.PAS` is a minimalist implementation of xUnit in Pascal style.
There are several assertions available, e.g. `AssertEquals`, `AssertEqualsStr`, `AssertTrue` etc.
It needs _Far Calls_ enabled in compiler options.

* First compile the unit `TPUNIT.PAS`.
* Then compile application `ROSE_T.PAS`.
* Run `ROSE_T` to run the tests.
