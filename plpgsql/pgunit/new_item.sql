DROP PROCEDURE IF EXISTS new_item;
CREATE PROCEDURE new_item(
      name item.name%TYPE,
      sell_in item.sell_in%TYPE,
      quality item.quality%TYPE
  )
LANGUAGE plpgsql
AS $$
BEGIN
  INSERT INTO item (name, sell_in, quality) VALUES (name, sell_in, quality);
END;
$$;
