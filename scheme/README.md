# Scheme port of the Gilded-Rose Kata

This is a (Gambit) R5RS Scheme port of the *Gilded-Rose-Kata*.

## Building and Running

```shell
gsi texttest-fixture.scm
```

## Unit Test

`assert.scm` is a minimalist implementation of xUnit in Scheme style.
There are two assertions available, e.g. `(assert=)` and `(assert-string=)`.

```shell
gsi gilded-rose-test.scm
```
