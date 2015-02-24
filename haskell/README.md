# Haskell port of the Gilded-Rose Kata

This is a Haskell port of the *Gilded-Rose-Kata*. For updates and pull-request
on this haskell port go to https://github.com/sheyll/gilded-rose-haskell

## Building and Running

Run `./install_deps.sh` initially, then `./test.sh` to execute the tests after
each refactoring.

To execute the program run `./run.sh [days]` where `[days]` denotes an optional
parameter for the number of days to simulate.

Tests are in `test/GildedRoseSpec.hs`. Refer to http://hspec.github.io/ for
more information about writing tests using `Hspec`.
