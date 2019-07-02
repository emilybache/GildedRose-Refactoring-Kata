exec DBMS_SESSION.RESET_PACKAGE;
set serveroutput on;
exec DBMS_OUTPUT.ENABLE(1000000);

exec ut.run(USER||':gilded_rose_tests'||'');