#!/bin/bash
#
# Rebuild the project and run the hspec based unit tests.  This could have been
# achieved by 'cabal test' but then the output would not be as colorful.
#
set -eu

cabal -v0 build
./dist/build/spec/spec $@
