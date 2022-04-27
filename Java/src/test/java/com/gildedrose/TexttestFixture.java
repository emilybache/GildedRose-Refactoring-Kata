package com.gildedrose;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

public class TexttestFixture {
    public static void main(String[] args) {
        System.out.println("OMGHAI!");

        Item[] items = new Item[]{
            new Item("+5 Dexterity Vest", 10, 20), //
            new Item("Aged Brie", 2, 0), //
            new Item("Elixir of the Mongoose", 5, 7), //
            new Item("Sulfuras, Hand of Ragnaros", 0, 80), //
            new Item("Sulfuras, Hand of Ragnaros", -1, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
            // this conjured item does not work properly yet
//                new Item("Conjured Mana Cake", 3, 6)
        };

        GildedRose app = new GildedRose(items);

        int days = 6;
        if (args.length > 0) {
            days = Integer.parseInt(args[0]) + 1;
        }
        List<String> output = new ArrayList<>();
        output.add("name, sellIn, quality");

        for (int i = 0; i < days; i++) {
            output.add("-------- day " + i + " --------");
            for (Item item : items) {
                output.add(item.toString());
            }
            app.updateQuality();
        }
        try {
            exportToCsv(output, "base-output.csv");
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private static void exportToCsv(List<String> data, String fileName) throws FileNotFoundException {
        File csvOutputFile = new File(fileName);
        try (PrintWriter pw = new PrintWriter(csvOutputFile)) {
            data.forEach(pw::println);
        }
    }

}
