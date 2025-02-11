package com.gildedrose;

import static com.gildedrose.ItemFactory.createItem;
import static java.lang.Integer.parseInt;

public class TexttestFixture {
    public static void main(String[] args) {
        System.out.println("OMGHAI!");

        var items = new Item[] {
                createItem("+5 Dexterity Vest", 10, 20), //
                createItem("Aged Brie", 2, 0), //
                createItem("Elixir of the Mongoose", 5, 7), //
                createItem("Sulfuras, Hand of Ragnaros", 0, 80), //
                createItem("Sulfuras, Hand of Ragnaros", -1, 80),
                createItem("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                createItem("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                createItem("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                // this conjured item does not work properly yet
                createItem("Conjured Mana Cake", 3, 6) };

        var app = new GildedRose(items);

        int days = 2;
        if (args.length > 0) {
            days = parseInt(args[0]) + 1;
        }

        for (int i = 0; i < days; i++) {
            System.out.println("-------- day " + i + " --------");
            System.out.println("name, sellIn, quality");
            for (var item : items) {
                System.out.println(item);
            }
            System.out.println();
            app.degradeItems();
        }
    }

}
