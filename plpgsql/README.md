# Setup
Create database structure: psql -f ./structure/create.sql
Load code into database psql -f ./code/update_quality.sql -d gilded_rose
Load test data into database: psql -f ./test/data/load.sql -d gilded_rose
Execute: CALL update_quality();
Check results: SELECT * FROM items;

# Test
No test code provided, only a sample of test data
TODO: [https://pgtap.org/](introduce TAP - xUnit for PL/pg SQL)

