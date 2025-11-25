package com.gildedrose;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class GildedRoseApprovalTest {

    @Test
    void oneDayRegressionTest() {
        Item[] items = {
            new Item("+5 Dexterity Vest", 10, 20),
            new Item("Aged Brie", 2, 0),
            new Item("Elixir of the Mongoose", 5, 7),
            new Item("Sulfuras, Hand of Ragnaros", 0, 80),
            new Item("Sulfuras, Hand of Ragnaros", -1, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
            new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
            new Item("Conjured Mana Cake", 3, 6)
        };

        GildedRose app = new GildedRose(items);
        app.updateQuality(); // simulate day 1

        String result = String.join("\n",
            items[0].toString(),
            items[1].toString(),
            items[2].toString(),
            items[3].toString(),
            items[4].toString(),
            items[5].toString(),
            items[6].toString(),
            items[7].toString(),
            items[8].toString()
        );
        System.out.println("result tessssssst\n"+result);

        // Vérifie CORRESPONDANCE EXACTE pour éviter de casser la logique
        assertEquals(
            "+5 Dexterity Vest, 9, 19\n" +
                "Aged Brie, 1, 1\n" +
                "Elixir of the Mongoose, 4, 6\n" +
                "Sulfuras, Hand of Ragnaros, 0, 80\n" +
                "Sulfuras, Hand of Ragnaros, -1, 80\n" +
                "Backstage passes to a TAFKAL80ETC concert, 14, 21\n" +
                "Backstage passes to a TAFKAL80ETC concert, 9, 50\n" +
                "Backstage passes to a TAFKAL80ETC concert, 4, 50\n" +
                "Conjured Mana Cake, 2, 5",
            result
        );
    }
}
