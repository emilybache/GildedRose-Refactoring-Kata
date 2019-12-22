# Requirements
You'll need:
- PostgreSQL database, version >= 11 because PROCEDURE keyword isn't supported before [version 11](https://www.postgresql.org/docs/11/release-11.html)
- OS user with local connection privilege to database on standard 5432 port 

To use remote / local dockerized database, add ``` --host --port --username```  parameters to plsql invocation.

# Setup
In shell:
- create database: ```createdb gilded_rose```
- create item table (structure): ```psql -d gilded_rose -f ./structure/create.sql``` 
- load code into database: ```psql -d gilded_rose -f ./code/update_quality.sql ```

If you get this message```LINE 1: CREATE OR REPLACE PROCEDURE public.update_quality()```, your PostgreSQL version may under 11, consider upgrading.

# Interactive run
In shell:
- load test data into database: ```psql -d gilded_rose -f ./test/data/load.sql```
- connect to CLI: ```psql -d gilded_rose```
- check item state: ```SELECT * FROM item;```
- execute item update: ```CALL update_quality();```
- check item state: ```SELECT * FROM item;```

# Test
No test code provided, only a sample of test data
TODO: [https://pgtap.org/](introduce TAP - xUnit for PL/pg SQL)

