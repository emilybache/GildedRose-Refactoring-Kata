# Requirements
You'll need:
- PostgreSQL database, version >= 11 because PROCEDURE keyword isn't supported before [version 11](https://www.postgresql.org/docs/11/release-11.html)
- OS user with local connection privilege to database on standard 5432 port 

To use remote / local dockerized database, add ``` --host --port --username```  parameters to plsql invocation.

# Setup
In shell:
- create database: ```createdb gilded_rose```
- create item table (structure): ```psql -d gilded_rose -f ./structure/create.sql``` 
- load code into database: ```psql -d gilded_rose -f ./src/update_quality.sql ```

If you get this message```LINE 1: CREATE OR REPLACE PROCEDURE public.update_quality()```, your PostgreSQL version may under 11, consider upgrading.

# Run 
In shell:
- load manual test data into database: ```psql -d gilded_rose -f ./test/manual/load.sql```
- connect to CLI: ```psql -d gilded_rose```
- check item state: ```SELECT * FROM item;```
- execute item update: ```CALL update_quality();```
- check item state: ```SELECT * FROM item;```
- empty table : ```TRUNCATE TABLE item;```


# Test

## Using pgTAP

### Requirements
Install pgTAP [instructions here](https://pgtap.org/documentation.html#installation)

```item``` table is supposed to be empty. 
If not, it would cause a (false positive)[https://en.wikipedia.org/wiki/False_positives_and_false_negatives]

### Execute
In shell, execute ```pg_prove --dbname gilded_rose test/pgtap/*.sql```. 
You should get ```test/pgtap/template.sql .. ok All tests successful.```

If you get this message ```ERROR: function plan(integer) does not exist LINE 1: SELECT PLAN(1);```, pgTAP is not working => check your pgTAP installation. 
If you get this message ```Failed test:  2 Parse errors: Bad plan.  You planned 1 tests but ran 2.```, the item table contains data, interfering with the test => empty it, then run test again.
