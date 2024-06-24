CREATE OR REPLACE PACKAGE texttest IS
   -- %suite(texttest)
   -- %suitepath(gilded_rose_tests)
   -- %rollback(manual)
   
   -- %beforeall
   PROCEDURE setup;
   
   -- %test(main test)
   PROCEDURE main_test; 
END texttest;
/