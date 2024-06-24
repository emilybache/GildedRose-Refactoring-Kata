CREATE OR REPLACE PROCEDURE new_item(
    i_name item.name%TYPE,
    i_sell_in item.sell_in%TYPE,
    i_quality item.quality%TYPE)
IS
BEGIN
  INSERT INTO item (name, sell_in, quality) VALUES (i_name, i_sell_in, i_quality);
END new_item;
/
