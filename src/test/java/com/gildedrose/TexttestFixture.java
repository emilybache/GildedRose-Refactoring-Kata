package com.gildedrose;

import com.gildedrose.utils.EncapsulatedItem;
import com.gildedrose.utils.EncapsulateItemBuilder;

public class TexttestFixture {
    public static void main(String[] args) {
        System.out.println("OMGHAI!");

        final EncapsulatedItem[] items = buildItems();

        GildedRose app = new GildedRose(items);

        int days = 2;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }

        for (int i = 0; i < days; i++) {
            System.out.println("-------- day " + i + " --------");
            System.out.println("name, sellIn, quality");
            for (EncapsulatedItem item : items) {
                System.out.println(item);
            }
            System.out.println();
            app.updateQuality();
        }
    }

    private static EncapsulatedItem[] buildItems() {
        return new EncapsulatedItem[] {
                new EncapsulateItemBuilder().named("+5 Dexterity Vest").toSellIn(10).ofQuality(20).build(),
                new EncapsulateItemBuilder().named("Aged Brie").toSellIn(2).ofQuality(0).build(),
                new EncapsulateItemBuilder().named("Elixir of the Mongoose").toSellIn(5).ofQuality(7).build(),
                new EncapsulateItemBuilder().named("Sulfuras, Hand of Ragnaros").toSellIn(0).ofQuality(80).build(),
                new EncapsulateItemBuilder().named("Sulfuras, Hand of Ragnaros").toSellIn(-1).ofQuality(80).build(),
                new EncapsulateItemBuilder().named("Backstage passes to a TAFKAL80ETC concert").toSellIn(15).ofQuality(20).build(),
                new EncapsulateItemBuilder().named("Backstage passes to a TAFKAL80ETC concert").toSellIn(10).ofQuality(49).build(),
                new EncapsulateItemBuilder().named("Backstage passes to a TAFKAL80ETC concert").toSellIn(5).ofQuality(49).build(),
                new EncapsulateItemBuilder().named("Conjured Mana Cake").toSellIn(3).ofQuality(6).build(),};

    }

}
