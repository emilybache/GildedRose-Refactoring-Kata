#!/bin/bash
#
# Fetch and build all dependencies
#
set -eu

cabal install --enable-tests --disable-optimization --dependencies-only
cabal -v0 configure --enable-tests --disable-optimization
