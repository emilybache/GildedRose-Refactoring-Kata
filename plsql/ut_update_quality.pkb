CREATE OR REPLACE PACKAGE BODY ut_update_quality
IS

  PROCEDURE ut_setup IS
  BEGIN
    DELETE FROM item;
  END;
  
  PROCEDURE ut_teardown IS
  BEGIN
    NULL;
  END;
  
  PROCEDURE ut_foo
  IS
    l_name item.name%TYPE;
  BEGIN
    new_item('foo', 0, 0);
   
    update_quality();
    
    SELECT name INTO l_name FROM item;
    utAssert.eq('name did change', l_name, 'fixme');
  END ut_foo;

END ut_update_quality;
/
