SET SERVEROUTPUT ON;

DELETE FROM item;

DECLARE
  l_days NUMBER(3);
  CURSOR c_items IS
    SELECT name, sell_in, quality FROM item;
  l_item c_items%ROWTYPE;
BEGIN
  DBMS_OUTPUT.PUT_LINE('OMGHAI!');

  new_item('+5 Dexterity Vest', 10, 20);
  new_item('Aged Brie', 2, 0);
  new_item('Elixir of the Mongoose', 5, 7);
  new_item('Sulfuras, Hand of Ragnaros', 0, 80);
  new_item('Sulfuras, Hand of Ragnaros', -1, 80);
  new_item('Backstage passes to a TAFKAL80ETC concert', 15, 20);
  new_item('Backstage passes to a TAFKAL80ETC concert', 10, 49);
  new_item('Backstage passes to a TAFKAL80ETC concert', 5, 49);
  -- this conjured item does not work properly yet ;
  new_item('Conjured Mana Cake', 3, 6);

  l_days := 2;

  FOR i IN 0 .. l_days - 1
  LOOP
    DBMS_OUTPUT.PUT_LINE('-------- day ' || TO_CHAR(i) || ' --------');
    DBMS_OUTPUT.PUT_LINE('name, sellIn, quality');
    FOR l_item IN c_items LOOP
      DBMS_OUTPUT.PUT_LINE(l_item.name || ', ' || l_item.sell_in || ', ' || l_item.quality);
    END LOOP;
    DBMS_OUTPUT.PUT_LINE('');
    update_quality();
  END LOOP;
END;
