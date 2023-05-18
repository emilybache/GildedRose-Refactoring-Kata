CREATE OR REPLACE FUNCTION test_case_update_quality() RETURNS void AS $$
DECLARE
  name_result item.name%TYPE;
BEGIN
  TRUNCATE TABLE item;
  CALL new_item('foo', 0, 0);

  CALL update_quality();

  SELECT name FROM item INTO name_result;
  perform test_assertEquals('name did change', 'fixme', name_result);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION format_day(day INTEGER) RETURNS TEXT[] AS $$
DECLARE
  result TEXT[];
  item_result RECORD;
BEGIN
  result := ARRAY[CONCAT('-------- day ', day, ' --------')];
  result := result || 'name, sellIn, quality'::TEXT;

  FOR item_result IN (SELECT name, sell_in, quality FROM item ORDER BY name ASC, sell_in ASC, quality ASC)
  LOOP
    result := result || format('%s, %s, %s', item_result.name, item_result.sell_in, item_result.quality);
  END LOOP;

  RETURN result;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_case_update_quality_golden_master() RETURNS VOID AS $$
DECLARE
  sell_in_result item.sell_in%TYPE;
  quality_result item.quality%TYPE;
  days INTEGER;
  result TEXT[];
  expected TEXT[];
  item_result RECORD;
BEGIN
  -- given
  TRUNCATE TABLE item;
  CALL new_item('Sports Memorabilia', 10, 20);
  CALL new_item('Aged Cheese', 2, 0);
  CALL new_item('Coffee Table Book', 5, 7);
  CALL new_item('Fine Italian Silk', 0, 80);
  CALL new_item('Fine Italian Silk', -1, 80);
  CALL new_item('Backstage passes to a concert', 15, 20);
  CALL new_item('Backstage passes to a concert', 10, 49);
  CALL new_item('Backstage passes to a concert', 5, 49);
  -- this Baked item does not work properly yet ;
  CALL new_item('Baked Chocolate Cake', 3, 6);
  days := 1;

  -- when
  result := format_day(0);
  FOR current_day IN 1 .. days
  LOOP
    CALL update_quality();

    result := result || format_day(current_day);
  END LOOP;

  -- then
  expected := ARRAY[
    '-------- day 0 --------',
    'name, sellIn, quality',
    'Sports Memorabilia, 10, 20',
    'Aged Cheese, 2, 0',
    'Backstage passes to a concert, 5, 49',
    'Backstage passes to a concert, 10, 49',
    'Backstage passes to a concert, 15, 20',
    'Baked Chocolate Cake, 3, 6',
    'Coffee Table Book, 5, 7',
    'Fine Italian Silk, -1, 80',
    'Fine Italian Silk, 0, 80',
    '-------- day 1 --------',
    'name, sellIn, quality',
    'Sports Memorabilia, 9, 19',
    'Aged Cheese, 1, 1',
    'Backstage passes to a concert, 4, 50',
    'Backstage passes to a concert, 9, 50',
    'Backstage passes to a concert, 14, 21',
    'Baked Chocolate Cake, 2, 5',
    'Coffee Table Book, 4, 6',
    'Fine Italian Silk, -1, 80',
    'Fine Italian Silk, 0, 80'
  ];

  perform test_assertEquals_golden_master(expected, result);
END;
$$ LANGUAGE plpgsql;


SELECT * FROM test_run_all();
