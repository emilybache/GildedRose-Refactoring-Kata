CREATE OR REPLACE PACKAGE BODY texttest IS
   co_lf   CONSTANT VARCHAR2(1) := CHR(10);

   PROCEDURE put_line(p_buffer IN OUT NOCOPY VARCHAR2, p_line VARCHAR2) IS
   BEGIN
      p_buffer := p_buffer || p_line || co_lf;
   END put_line;

   PROCEDURE setup IS
   BEGIN
      DELETE FROM ITEM;

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
   END setup;

   PROCEDURE main_test IS
      v_result     VARCHAR2(4000) := '';

      v_expected   VARCHAR2(4000) := '';

      l_days       NUMBER(3);

      CURSOR c_items IS SELECT name, sell_in, quality FROM item;

      l_item       c_items%ROWTYPE;
   BEGIN
      put_line(v_expected, 'OMGHAI!');
      put_line(v_expected, '-------- day 0 --------');
      put_line(v_expected, 'name, sellIn, quality');
      put_line(v_expected, '+5 Dexterity Vest, 10, 20' || co_lf || 'Aged Brie, 2, 0');
      put_line(v_expected, 'Elixir of the Mongoose, 5, 7');
      put_line(v_expected, 'Sulfuras, Hand of Ragnaros, 0, 80');
      put_line(v_expected, 'Sulfuras, Hand of Ragnaros, -1, 80');
      put_line(v_expected, 'Backstage passes to a TAFKAL80ETC concert, 15, 20');
      put_line(v_expected, 'Backstage passes to a TAFKAL80ETC concert, 10, 49');
      put_line(v_expected, 'Backstage passes to a TAFKAL80ETC concert, 5, 49');
      put_line(v_expected, 'Conjured Mana Cake, 3, 6');
      put_line(v_expected, '-------- day 1 --------');
      put_line(v_expected, 'name, sellIn, quality');
      put_line(v_expected, '+5 Dexterity Vest, 9, 19');
      put_line(v_expected, 'Aged Brie, 1, 1');
      put_line(v_expected, 'Elixir of the Mongoose, 4, 6');
      put_line(v_expected, 'Sulfuras, Hand of Ragnaros, 0, 80');
      put_line(v_expected, 'Sulfuras, Hand of Ragnaros, -1, 80');
      put_line(v_expected, 'Backstage passes to a TAFKAL80ETC concert, 14, 21');
      put_line(v_expected, 'Backstage passes to a TAFKAL80ETC concert, 9, 50');
      put_line(v_expected, 'Backstage passes to a TAFKAL80ETC concert, 4, 50');
      put_line(v_expected, 'Conjured Mana Cake, 2, 5');

      put_line(v_result, 'OMGHAI!');
      l_days := 2;

      FOR i IN 0 .. l_days - 1
      LOOP
         put_line(v_result, '-------- day ' || TO_CHAR(i) || ' --------');
         put_line(v_result, 'name, sellIn, quality');

         FOR l_item IN c_items
         LOOP
            put_line(v_result, l_item.name || ', ' || l_item.sell_in || ', ' || l_item.quality);
         END LOOP;

         update_quality();
      END LOOP;

      ut.expect(v_result).to_equal(v_expected);
   END;
END texttest;
/