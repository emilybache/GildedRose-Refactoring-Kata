# Haskell port of the Gilded-Rose Kata

This is a Haskell port of the *Gilded-Rose-Kata*. 

## Prerequisite

If you don't have a recent Stack version installed in your system, follow the 
[installation instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/) 
for your operating system. 

## Building and Running

Run `stack build` initially, then `stack test` to execute the tests after
each refactoring.

To execute the program run `stack run [days]` where `[days]` denotes an optional
parameter for the number of days to simulate.

Tests are in `test/GildedRoseSpec.hs`. Refer to http://hspec.github.io/ for
more information about writing tests using `Hspec`.
