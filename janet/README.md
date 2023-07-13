# Gilded Rose

This is the Gilded Rose kata in [janet](https://janet-lang.org).

## Getting started

We'll need to install [janet and jpm](https://janet-lang.org/docs/index.html).

## Running texttest

To produce output suitable for texttest, we can run:

```sh
janet test/texttest.janet
```

We can specify the number of days as an argument:

```sh
janet test/texttest.janet 30
```

## Running tests

First, we install the test library dependency using `jpm -l deps`. Then we can run all the tests using:

```sh
jpm -l test
```

The testing library being used is [judge](https://github.com/ianthehenry/judge), that enables the writing of inline snapshot tests. 
Specifically, Judge has the ability to capture a snapshot of stdout, which gives us a similar experience to that offered by texttest, but natively in the janet tests.

Having run `jpm -l deps`, we now have a (repository-local) installed executable of judge, which we can run directly using:

```sh
./jpm_tree/bin/judge
```
(pass `-h` for usage).
