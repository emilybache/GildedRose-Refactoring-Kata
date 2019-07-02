CREATE OR REPLACE PACKAGE BODY ut_update_quality IS
   PROCEDURE cleanup_before_each IS
   BEGIN
      DELETE FROM item;
   END;

   PROCEDURE ut_foo IS
      l_name   item.name%TYPE;
   BEGIN
      new_item('foo', 0, 0);

      update_quality();

      SELECT name INTO l_name FROM item;

      ut.expect(l_name, a_message => 'name did change').to_equal('fixme');
   END ut_foo;
END ut_update_quality;
/