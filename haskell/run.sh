#!/bin/bash
#
# Rebuild the project and run Main.main with all arguments passed to this
# script.
#
set -eu

cabal -v0 run $@
