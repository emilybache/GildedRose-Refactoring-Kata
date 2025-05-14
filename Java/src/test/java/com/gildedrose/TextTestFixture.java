package com.gildedrose;

import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class TextTestFixture {
    public static void main(String[] args) {
        log.info("OMGHAI!");

        List<Item> items = List.of(
                new Item("+5 Dexterity Vest", 10, 20), //
                new Item("Aged Brie", 2, 0), //
                new Item("Elixir of the Mongoose", 5, 7), //
                new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
                new Item("Sulfuras, Hand of Ragnaros", -1, 80),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                // this conjured item does not work properly yet
                new Item("Conjured Mana Cake", 3, 6));

        GildedRose app = new GildedRose(items);

        int days = 12;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }

        for (int i = 0; i < days; i++) {
            log.info("-------- day {} --------", i);
            log.info("name, sellIn, quality");
            for (Item item : items) {
                log.info(item.toString());
            }
            app.updateQuality();
        }
    }

}
