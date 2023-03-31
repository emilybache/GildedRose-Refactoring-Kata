package com.gildedrose;

import com.gildedrose.logger.CustomLogger;

public class TexttestFixture {
    public static void main(String[] args) {
    	
    	CustomLogger.getLogger().info("OMGHAI!");

        Item[] items = new Item[] {
                new Item("+5 Dexterity Vest", 10, 20), //
                new Item("Aged Brie", 2, 0), //
                new Item("Elixir of the Mongoose", 5, 7), //
                new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
                new Item("Sulfuras, Hand of Ragnaros", -1, 80),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                new Item("Conjured Mana Cake", 3, 6) };

        GildedRose app = new GildedRose(items);

        int days = 2;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }

        for (int i = 0; i < days; i++) {
        	CustomLogger.getLogger().info("-------- day " + i + " --------");
        	CustomLogger.getLogger().info("name, sellIn, quality");
            for (Item item : items) {
                CustomLogger.getLogger().info(item.toString());
            }

            app.updateQuality();
        }
    }

}
