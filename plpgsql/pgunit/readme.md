## Requirement
Testing on postgres 12  
Unit test framework used : pgunit (https://github.com/adrianandrei-ca/pgunit)  

## Setup
Run `docker-compose up -d` to start, and `docker-compose exec database bash` to enter in container.  
You can run `cat update_quality.sql run_tests.sql | psql -d kata -f -`

## Kata
`update_quality.sql` contains code to refactor, and `run_tests.sql` contains test examples.
