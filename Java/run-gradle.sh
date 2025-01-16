#!/bin/sh

./gradlew clean build
java -cp build/classes/java/main:build/classes/java/test com.gildedrose.TexttestFixture 30 > build/result.txt
diff build/result.txt ../texttests/ThirtyDays/stdout.gr
