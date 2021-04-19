package com.gildedrose;

public class TestItems {
    public static void main(String[] args) {
        System.out.println("Start Testing");

        Item[] items = new Item[] {
                new AgedBrie("Aged Brie", 2, 0), //
                new Sulfuras("Sulfuras", 10, 5), //
                new BackStagePass("BackStagePass", 20, 20)
                
                
        };

        GildedRose gildedRose = new GildedRose(items);

        int days = 3;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }

        for (int i = 0; i < days; i++) {
            System.out.println("-------- day " + i + " --------");
            System.out.println("name, sellIn, quality");
            for (Item item : items) {
                System.out.println(item);
                System.out.println();
                gildedRose.updateQuality();
            }
        }
    }

}
