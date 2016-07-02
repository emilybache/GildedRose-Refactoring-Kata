package com.gildedrose;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;

public class TexttestFixture {
    public static void main(String[] args) {
        int days = getDays(args);
        updateQuality(days, System.out);
    }

    public static void updateQualityOverTenDays(File outputFile) throws FileNotFoundException {
        int days = 10;
        updateQuality(days, new PrintStream(outputFile));
    }

    private static void updateQuality(int days, PrintStream out) {
        out.println("OMGHAI!");

        Item[] items = new Item[] {
                new Item("+5 Dexterity Vest", 10, 20), //
                new Item("Aged Brie", 2, 0), //
                new Item("Elixir of the Mongoose", 5, 7), //
                new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
                new Item("Sulfuras, Hand of Ragnaros", -1, 80),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
                // this conjured item does not work properly yet
                new Item("Conjured Mana Cake", 3, 6) };

        GildedRose app = new GildedRose(items);

        for (int i = 0; i < days; i++) {
            out.println("-------- day " + i + " --------");
            out.println("name, sellIn, quality");
            for (Item item : items) {
                out.println(item);
            }
            out.println();
            app.updateQuality();
        }
    }

    private static int getDays(String[] args) {
        int days = 2;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }
        return days;
    }
}
