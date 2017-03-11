#!/bin/bash

OUTFILE=/tmp/golden-dross.txt

./golden_rose > $OUTFILE
diff $OUTFILE golden_rule.txt
git diff GildedRose.h
