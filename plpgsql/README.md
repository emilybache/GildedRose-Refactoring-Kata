# Requirements
You'll need:
- PostgreSQL database, version >= 11 because PROCEDURE keyword isn't supported before [version 11](https://www.postgresql.org/docs/11/release-11.html)
- OS user with local connection privilege to database on standard 5432 port 

To use remote / local dockerized database, add ``` --host --port --username```  parameters to plsql invocation.

# Setup
## With docker
Run `docker-compose up -d <TEST_FRAMEWORK>` to start, and `docker-compose exec <TEST_FRAMEWORK> bash` to enter in container.  
`<TEST_FRAMEWORK>` is the testing framework name. Two values are possible: `pgunit` or `pgtap`

## Without docker
In shell:
- create database: ```createdb kata```
- create item table (structure): ```psql -d kata -f ./src/item.sql``` 
- create procedure to help to add item: ```psql -d kata -f ./src/new_item.sql``` 
- load code into database: ```psql -d kata -f ./src/update_quality.sql ```

If you get this message```LINE 1: CREATE OR REPLACE PROCEDURE public.update_quality()```, your PostgreSQL version may under 11, consider upgrading.

# Run
In shell:
- connect to CLI: ```psql -d kata```
- add item: ```CALL new_item('+5 Dexterity Vest', 10, 20);```
- check item state: ```SELECT * FROM item;```
- execute item update: ```CALL update_quality();```
- check item state: ```SELECT * FROM item;```
- empty table : ```TRUNCATE TABLE item;```

# Kata
`src/update_quality.sql` contains code to refactor.

# Test

## Using pgTAP
### Requirements
Install pgTAP [instructions here](https://pgtap.org/documentation.html#installation)  
It's already installed with docker

```item``` table is supposed to be empty. 
If not, it would cause a (false positive)[https://en.wikipedia.org/wiki/False_positives_and_false_negatives]

### Execute
Run `docker-compose up -d pgtap` to start, and `docker-compose exec pgtap bash` to enter in container.  
In shell, execute ```psql -d kata -f src/update_quality.sql && pg_prove pgtap/test_*.sql```. 
You should get ```pgtap/test_case_update_quality.sql .. ok All tests successful.```

If you get this message ```ERROR: function plan(integer) does not exist LINE 1: SELECT PLAN(1);```, pgTAP is not working => check your pgTAP installation. 
If you get this message ```Failed test:  2 Parse errors: Bad plan.  You planned 1 tests but ran 2.```, the item table contains data, interfering with the test => empty it, then run test again.

`pgtap/test_case_update_quality.sql` contains test examples.

## Using pgunit
### Requirement
Unit test framework used : pgunit (https://github.com/adrianandrei-ca/pgunit)  
It's already installed with docker  

### Execute
Run `docker-compose up -d pgunit` to start, and `docker-compose exec pgunit bash` to enter in container.  
You can run `cat src/update_quality.sql pgunit/run_tests.sql | psql -d kata -f -`

`pgunit/run_tests.sql` contains test examples.
