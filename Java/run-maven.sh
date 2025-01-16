#!/bin/sh

./mvnw clean package
java -cp target/classes:target/test-classes com.gildedrose.TexttestFixture 30 > target/result.txt
diff target/result.txt ../texttests/ThirtyDays/stdout.gr
