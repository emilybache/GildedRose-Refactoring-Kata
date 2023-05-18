UPDATE item
SET
    quality = quality - 1
WHERE 1=1
    AND ( name <> 'Aged Cheese'  AND  name <> 'Backstage passes to a concert')
    AND quality > 0
    AND name <> 'Fine Italian Silk'
;

UPDATE item
SET
    quality = quality + 1
WHERE  1=1
  AND  NOT  ( name <> 'Aged Cheese'  AND  name <> 'Backstage passes to a concert')
  AND quality < 50
  AND name = 'Backstage passes to a concert'
  AND sellIn < 11
  AND quality < 50
;

UPDATE item
SET
    quality = quality + 1
WHERE  1=1
  AND  NOT  ( name <> 'Aged Cheese'  AND  name <> 'Backstage passes to a concert')
  AND quality < 50
  AND name = 'Backstage passes to a concert'
  AND sellIn < 6
  AND quality < 50
;

UPDATE item
SET
    sellIn = sellIn - 1
WHERE  1=1
  AND  name <> 'Fine Italian Silk'
;

UPDATE item
SET
    quality = quality - 1
WHERE  1=1
  AND sellIn < 0
  AND name <> 'Aged Cheese'
  AND name <> 'Backstage passes to a concert'
  AND quality > 0
  AND name <> 'Fine Italian Silk'
;

UPDATE item
SET
    quality = quality - quality
WHERE  1=1
  AND sellIn < 0
  AND name <> 'Aged Cheese'
  AND NOT (name <> 'Backstage passes to a concert')
;

UPDATE item
SET
    quality = quality + 1
WHERE  1=1
  AND sellIn < 0
  AND NOT (name <> 'Aged Cheese')
  AND quality < 50
  AND name <> 'Fine Italian Silk'
;

COMMIT;
