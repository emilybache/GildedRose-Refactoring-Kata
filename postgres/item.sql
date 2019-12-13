DROP TABLE IF EXISTS item;
CREATE TABLE item
  (
    name    character varying(100) NOT NULL,
    sell_in numeric(6) NOT NULL,
    quality numeric(6) NOT NULL
  );

