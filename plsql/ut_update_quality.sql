-- unit test using utPLSQL 2.2.1, see http://utplsql.sourceforge.net/ ;
-- test package must be named like the procedure we want to test ;

EXEC utplsql.test ('update_quality', recompile_in => FALSE);
-- check DBMS_OUTPUT for output ;
