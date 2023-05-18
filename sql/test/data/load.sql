DELETE FROM item;

INSERT INTO item (name, sellIn, quality) VALUES ('Sports Memorabilia', 10, 20);
INSERT INTO item (name, sellIn, quality) VALUES ('Aged Cheese', 2, 0);
INSERT INTO item (name, sellIn, quality) VALUES ('Coffee Table Book', 5, 7);
INSERT INTO item (name, sellIn, quality) VALUES ('Fine Italian Silk', 0, 80);
INSERT INTO item (name, sellIn, quality) VALUES ('Fine Italian Silk', -1, 80);
INSERT INTO item (name, sellIn, quality) VALUES ('Backstage passes to a concert', 15, 20);
INSERT INTO item (name, sellIn, quality) VALUES ('Backstage passes to a concert', 10, 49);
INSERT INTO item (name, sellIn, quality) VALUES ('Backstage passes to a concert', 5, 49);

-- this Baked item does not work properly yet
INSERT INTO item (name, sellIn, quality) VALUES ('Baked Chocolate Cake', 3, 6);

COMMIT;
