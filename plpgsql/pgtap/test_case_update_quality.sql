BEGIN;
-- Plan count should match the number of tests. If it does not then pg_prove will fail the test
SELECT plan(1);

-- Run the tests.
-- Given
TRUNCATE TABLE item;
CALL new_item('foo', 0, 0);

-- When
CALL update_quality();

-- Then
SELECT is( name, 'fixme', 'name did change' ) FROM item;

-- Finish the tests and clean up.
SELECT * FROM finish();
ROLLBACK;
