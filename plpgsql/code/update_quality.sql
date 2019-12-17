CREATE OR REPLACE PROCEDURE public.update_quality()
  LANGUAGE plpgsql
AS $$

DECLARE

    rec_item   RECORD;
    cur_items  CURSOR FOR SELECT * FROM item;
    
BEGIN

OPEN cur_items;
    
    
   LOOP

      FETCH cur_items INTO rec_item;
      EXIT WHEN NOT FOUND;
 
      IF (rec_item.name <> 'Aged Brie' AND rec_item.name <> 'Backstage passes to a TAFKAL80ETC concert') THEN
        IF (rec_item.quality > 0) THEN
          IF (rec_item.name <> 'Sulfuras, Hand of Ragnaros') THEN
            rec_item.quality = rec_item.quality - 1;
          END IF;
        END IF;
      ELSE
        IF (rec_item.quality < 50) THEN
          rec_item.quality = rec_item.quality + 1;
          IF (rec_item.name = 'Backstage passes to a TAFKAL80ETC concert') THEN
            IF (rec_item.sellIn < 11)  THEN
              IF (rec_item.quality < 50)  THEN
                rec_item.quality = rec_item.quality + 1;
              END IF;
            END IF;
            IF (rec_item.sellIn < 6) THEN
              IF (rec_item.quality < 50) THEN
                rec_item.quality = rec_item.quality + 1;
              END IF;
            END IF;
          END IF;
        END IF;
      END IF;
    
      IF (rec_item.name <> 'Sulfuras, Hand of Ragnaros') THEN
        rec_item.sellIn = rec_item.sellIn - 1;
      END IF;
      
      IF (rec_item.sellIn < 0) THEN
        IF (rec_item.name <> 'Aged Brie') THEN
          IF (rec_item.name <> 'Backstage passes to a TAFKAL80ETC concert') THEN
            IF (rec_item.quality > 0) THEN
              IF (rec_item.name <> 'Sulfuras, Hand of Ragnaros') THEN
                rec_item.quality = rec_item.quality - 1;
              END IF;
            END IF;
          ELSE
            rec_item.quality = rec_item.quality - rec_item.quality;
          END IF;
        ELSE
          IF (rec_item.quality < 50) THEN
            rec_item.quality = rec_item.quality + 1;
          END IF;
        END IF;
      END IF;
      
      UPDATE item 
      SET 
        quality = rec_item.quality,
        sellIn  = rec_item.sellIn
      WHERE 
        name = rec_item.name;

   END LOOP;
  
   CLOSE cur_items;
   
  COMMIT;
   
END;
$$
