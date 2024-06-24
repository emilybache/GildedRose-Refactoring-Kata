CREATE OR REPLACE PROCEDURE update_quality
IS
  CURSOR c_items IS
    SELECT name, sell_in, quality FROM item FOR UPDATE;
  l_item c_items%ROWTYPE;
  l_name item.name%TYPE;
  l_sell_in item.sell_in%TYPE;
  l_quality item.quality%TYPE;
BEGIN
  FOR l_item IN c_items
  LOOP
    l_name    := l_item.name;
    l_sell_in := l_item.sell_in;
    l_quality := l_item.quality;

    IF l_name <> 'Aged Brie' AND l_name <> 'Backstage passes to a TAFKAL80ETC concert' THEN
      IF l_quality > 0 THEN
        IF l_name <> 'Sulfuras, Hand of Ragnaros' THEN
          l_quality := l_quality - 1;
        END IF;
      END IF;
    ELSE
      IF (l_quality < 50) THEN
        l_quality := l_quality + 1;
        IF l_name = 'Backstage passes to a TAFKAL80ETC concert' THEN
          IF l_sell_in < 11 THEN
            IF l_quality < 50 THEN
              l_quality := l_quality + 1;
            END IF;
          END IF;
          IF l_sell_in < 6 THEN
            IF l_quality < 50 THEN
              l_quality := l_quality + 1;
            END IF;
          END IF;
        END IF;
      END IF;
    END IF;

    IF l_name <> 'Sulfuras, Hand of Ragnaros' THEN
      l_sell_in := l_sell_in - 1;
    END IF;

    IF l_sell_in < 0 THEN
      IF l_name <> 'Aged Brie' THEN
        IF l_name <> 'Backstage passes to a TAFKAL80ETC concert' THEN
          IF l_quality > 0 THEN
            IF l_name <> 'Sulfuras, Hand of Ragnaros' THEN
              l_quality := l_quality - 1;
            END IF;
          END IF;
        ELSE
          l_quality := l_quality - l_quality;
        END IF;
      ELSE
        IF l_quality < 50 THEN
          l_quality := l_quality + 1;
        END IF;
      END IF;
    END IF;

    UPDATE item
      SET name = l_name, sell_in = l_sell_in, quality = l_quality WHERE CURRENT OF c_items;
  END LOOP;
END update_quality;
/
