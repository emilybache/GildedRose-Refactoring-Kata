package com.gildedrose.items;

import com.gildedrose.main.GildedRose;
import com.gildedrose.main.Item;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static java.util.Arrays.asList;

@TestMethodOrder(OrderAnnotation.class)
class MultipleItemsTest {

    protected static final String BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert";
    protected static final String LEGENDARY_ITEM_NAME = "Sulfuras, Hand of Ragnaros";

    private final Item normalItem1 = new Item("+5 Dexterity Vest", 10, 20);
    private final Item agedBrieItem = new Item("Aged Brie", 2, 0);
    private final Item normalItem2 = new Item("Elixir of the Mongoose", 5, 7);
    private final Item legendaryItem1 = new Item(LEGENDARY_ITEM_NAME, 0, 80);
    private final Item legendaryItem2 = new Item(LEGENDARY_ITEM_NAME, -1, 80);
    private final Item backStageItem1 = new Item(BACKSTAGE_PASS, 15, 20);
    private final Item backStageItem2 = new Item(BACKSTAGE_PASS, 10, 49);
    private final Item backStageItem3 = new Item(BACKSTAGE_PASS, 5, 49);
    private final Item conjuredItem = new Item("Conjured Mana Cake", 3, 6);

    public List<Item> getOriginalTestItems() {
        return asList(normalItem1, agedBrieItem, normalItem2, legendaryItem1, legendaryItem2,
            backStageItem1, backStageItem2, backStageItem3, conjuredItem);
    }

    @Test
    void test() {
        List<Item> items = getOriginalTestItems();
        GildedRose gildedRose = new GildedRose(items);
        int days = 20;
        for (int i = 0; i < days; i++) {
            for (Item item : items) {
                System.out.println(item);
            }
            gildedRose.updateQuality();
        }
    }

}
