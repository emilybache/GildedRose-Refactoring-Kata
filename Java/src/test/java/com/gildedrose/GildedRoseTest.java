package com.gildedrose;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {

    @Test
    void foo() {
        final String itemName = "foo";
        final Item[] items = new Item[]{createItem("foo", 0, 0)};
        final GildedRose app = new GildedRose(items);
        app.updateQuality();

        validateItem(items[0], itemName, -1, 0);
    }

    @Test
    public void testUpdateQualityConjuredItem() {
        final String itemName = "Conjured";
        final Item[] items = new Item[]{createItem(itemName, 4, 8)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();
        validateItem(items[0], itemName, 3, 6);
    }

    @Test
    public void testUpdateQualitySulfuras() {
        final String itemName = "Sulfuras";
        final Item[] items = new Item[]{new Item(itemName, 0, 80)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        validateItem(items[0], itemName, -1, 80);
    }

    @Test
    public void testUpdateQualityBackstagePassesSelinDayLessThanZero() {
        final String itemName = "Backstage passes to a TAFKAL80ETC concert";
        final Item[] items = new Item[]{new Item(itemName, 0, 50)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        validateItem(items[0], itemName, -1, 0);
    }

    @Test
    public void testUpdateQualityBackstagePassesLessThanSixDays() {
        final String itemName = "Backstage passes to a TAFKAL80ETC concert";
        final Item[] items = new Item[]{new Item(itemName, 5, 40)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        validateItem(items[0], itemName, 4, 43);
    }

    @Test
    public void testUpdateQualityBackstagePassesGreaterThanElevenDays() {
        final String itemName = "Backstage passes to a TAFKAL80ETC concert";
        final Item[] items = new Item[]{new Item(itemName, 12, 40)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        validateItem(items[0], itemName, 11, 41);
    }

    @Test
    public void testUpdateQualityAgedBrieSelinGreaterThanZero() {
        final String itemName = "Backstage passes to a TAFKAL80ETC concert";
        final Item[] items = new Item[]{new Item(itemName, 12, 39)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        validateItem(items[0], itemName, 11, 40);
    }

    @Test
    public void testUpdateQualityAgedBrieSelinEqualToZero() {
        final String itemName = "Aged Brie";
        final Item[] items = new Item[]{new Item(itemName, 0, 39)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        validateItem(items[0], itemName, -1, 41);
    }

    @Test
    public void testUpdateQualityDefaultSelinEqualToZero() {
        final String itemName = "Another foo";
        final Item[] items = new Item[]{new Item(itemName, 0, 40)};
        final GildedRose gildedRose = new GildedRose(items);

        gildedRose.updateQuality();

        validateItem(items[0], itemName, -1, 38);
    }

    private Item createItem(final String name, final int sellIn, final int quality) {
        return new Item(name, sellIn, quality);
    }

    private void validateItem(final Item item, final String expectedName, final int expectedSellIn,
                              final int expectedQuality) {
        assertEquals(expectedName, item.name, "The name must be " + expectedName);
        assertEquals(expectedSellIn, item.sellIn, "The sellIn must be " + expectedSellIn);
        assertEquals(expectedQuality, item.quality, "The quality must be " + expectedQuality);
    }
}
