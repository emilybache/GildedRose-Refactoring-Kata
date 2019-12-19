UPDATE item
SET
    quality = quality - 1
WHERE 1=1
    AND ( name <> 'Aged Brie'  AND  name <> 'Backstage passes to a TAFKAL80ETC concert')
    AND quality > 0
    AND name <> 'Sulfuras, Hand of Ragnaros'
;

UPDATE item
SET
    quality = quality + 1
WHERE  1=1
  AND  NOT  ( name <> 'Aged Brie'  AND  name <> 'Backstage passes to a TAFKAL80ETC concert')
  AND quality < 50
  AND name = 'Backstage passes to a TAFKAL80ETC concert'
  AND sellIn < 11
  AND quality < 50
;

UPDATE item
SET
    quality = quality + 1
WHERE  1=1
  AND  NOT  ( name <> 'Aged Brie'  AND  name <> 'Backstage passes to a TAFKAL80ETC concert')
  AND quality < 50
  AND name = 'Backstage passes to a TAFKAL80ETC concert'
  AND sellIn < 6
  AND quality < 50
;

UPDATE item
SET
    sellIn = sellIn - 1
WHERE  1=1
  AND  name <> 'Sulfuras, Hand of Ragnaros'
;

UPDATE item
SET
    quality = quality - 1
WHERE  1=1
  AND sellIn < 0
  AND name <> 'Aged Brie'
  AND name <> 'Backstage passes to a TAFKAL80ETC concert'
  AND quality > 0
  AND name <> 'Sulfuras, Hand of Ragnaros'
;

UPDATE item
SET
    quality = quality - quality
WHERE  1=1
  AND sellIn < 0
  AND name <> 'Aged Brie'
  AND NOT (name <> 'Backstage passes to a TAFKAL80ETC concert')
;

UPDATE item
SET
    quality = quality + 1
WHERE  1=1
  AND sellIn < 0
  AND NOT (name <> 'Aged Brie')
  AND quality < 50
  AND name <> 'Sulfuras, Hand of Ragnaros'
;

COMMIT;