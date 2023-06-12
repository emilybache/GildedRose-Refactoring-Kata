package com.gildedrose;

import com.gildedrose.config.ProductFactory;
import com.gildedrose.enums.ProductType;
import com.gildedrose.generic.ItemType;
import com.gildedrose.logger.GildedRoseLogger;

public class TexttestFixture {
    public static void main(String[] args) {
    	GildedRoseLogger.getLogger().info("OMGHAI!");

        Item[] items = new Item[] {
                new Item("+5 Dexterity Vest", 2, 84),
                new Item("Aged Brie", 5, 10),
                new Item("Elixir of the Mongoose", 5, 7),
                new Item("Sulfuras, Hand of Ragnaros", 0, 80),
                new Item("Sulfuras, Hand of Ragnaros", -1, 80),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                new Item("Conjured Mana Cake", 10, 14) 
                };

        int days = 20;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }

        for (int i = 0; i < days; i++) {
        	GildedRoseLogger.getLogger().info("-------- day " + i + " --------");
        	GildedRoseLogger.getLogger().info("name, sellIn, quality");
        	for (Item item : items) {
        		ItemType itemType = ProductFactory.get(ProductType.getEnumByString(item.name));
        		GildedRoseLogger.getLogger().info(item.toString());
        		itemType.updateQuality(item);
            }
        }
    }

}
