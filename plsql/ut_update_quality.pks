CREATE OR REPLACE PACKAGE ut_update_quality
IS
   -- %suite(UT_REGRESSION_TEST)
   -- %suitepath(gilded_rose_tests)
   -- %rollback(manual)
   
  -- %beforeeach
  PROCEDURE cleanup_before_each;
  
  -- %test(Foo test)
  PROCEDURE ut_foo;
END ut_update_quality;
/
