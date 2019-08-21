package com.gildedrose;

import static org.junit.Assert.*;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class GildedRoseTest {

    /*
     First an introduction to our system:

	- All items have a SellIn value which denotes the number of days we have to sell the item
	- All items have a Quality value which denotes how valuable the item is
	- At the end of each day our system lowers both values for every item

Pretty simple, right? Well this is where it gets interesting:

	- Once the sell by date has passed, Quality degrades twice as fast
	- The Quality of an item is never negative
	- "Aged Brie" actually increases in Quality the older it gets
	- The Quality of an item is never more than 50
	- "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
	- "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
	Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
	Quality drops to 0 after the concert

    We have recently signed a supplier of conjured items. This requires an update to our system:

        - "Conjured" items degrade in Quality twice as fast as normal items

    Feel free to make any changes to the UpdateQuality method and add any new code as long as everything
    still works correctly. However, do not alter the Item class or Items property as those belong to the
    goblin in the corner who will insta-rage and one-shot you as he doesn't believe in shared code
    ownership (you can make the UpdateQuality method and Items property static if you like, we'll cover
    for you).

    Just for clarification, an item can never have its Quality increase above 50, however "Sulfuras" is a
    legendary item and as such its Quality is 80 and it never alters.

     */
    private final Item sulfuras = new Item("Sulfuras, Hand of Ragnaros", 0, 80);

    private final Item standardWithHighestQuality = new Item("Standard", 5, 50);
    private final Item agedBrieWithHighestQuality = new Item("Aged Brie", 5, 50);
    private final Item conjuredWithHighestQuality = new Item("Conjured Mana Cake", 5, 50);
    private final Item backstageWithHighestQuality = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 50);

    private final Item standardWithLowQuality = new Item("Standard", 0, 1);
    private final Item agedBrieWithLowQuality = new Item("Aged Brie", 0, 1);
    private final Item conjuredWithLowQuality = new Item("Conjured Mana Cake", 0, 1);
    private final Item backstageWithLowQuality = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 1);

    @Test
    public void standardItem_shouldDegradeNormal_whenSellByDateNotPassed() {
        final int originalQuality = 40;
        final Item[] items = new Item[] { new Item("Standard", 4, originalQuality) };
        final GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Standard", app.items[0].name);
        assertEquals(3, app.items[0].sellIn);
        assertEquals(originalQuality - 1, app.items[0].quality);
    }

    @Test
    public void standardItem_shouldDegradeTwiceAsFast_whenSellByDatePassed() {
        final int originalQuality = 40;
        final Item[] items = new Item[] { new Item("Standard", 0, originalQuality) };
        final GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Standard", app.items[0].name);
        assertEquals(-1, app.items[0].sellIn);
        assertEquals(originalQuality - 2, app.items[0].quality);

        app.updateQuality();
        assertEquals(-2, app.items[0].sellIn);
        assertEquals(originalQuality - 4, app.items[0].quality);
    }


    @Test
    public void sulfurus_shouldNeverDegradeAndBeSold() {
        final Item[] items = new Item[] { sulfuras};
        final GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("Sulfuras, Hand of Ragnaros", app.items[0].name);
        assertEquals(0, app.items[0].sellIn);
        assertEquals(80, app.items[0].quality);
    }

    @Test
    public void qualityOfItem_exceptLegendaryItems_cantBeMoreThan_50() {
        final Item[] items = new Item[] {standardWithHighestQuality, backstageWithHighestQuality,
                agedBrieWithHighestQuality, conjuredWithHighestQuality};
        final GildedRose app = new GildedRose(items);
        app.updateQuality();
        final Optional<Item> qualityHigherThan80 = Arrays.stream(app.items).filter(item -> item.quality > 50).findAny();

        assertFalse(qualityHigherThan80.isPresent());
    }

    @Test
    public void qualityOfItem_onlyLegendaryItems_cantBeMoreThan_50() {
        final Item[] items = new Item[] { sulfuras, standardWithHighestQuality, backstageWithHighestQuality,
                agedBrieWithHighestQuality, conjuredWithHighestQuality};
        final GildedRose app = new GildedRose(items);
        app.updateQuality();
        final List<Item> qualityHigherThan80List = Arrays.stream(app.items).filter(item -> item.quality > 50).collect(Collectors.toList());

        assertEquals(1, qualityHigherThan80List.size());
        assertEquals(sulfuras.name, qualityHigherThan80List.get(0).name);
    }

    @Test
    public void qualityOfItem_canNeverBeNegative() {
        final Item[] items = new Item[] {sulfuras, standardWithLowQuality, backstageWithLowQuality,
                agedBrieWithLowQuality, conjuredWithLowQuality};
        final GildedRose app = new GildedRose(items);
        app.updateQuality();
        final Optional<Item> qualityNegative = Arrays.stream(app.items).filter(item -> item.quality < 0 ).findAny();

        assertFalse(qualityNegative.isPresent());

        app.updateQuality();
        final Optional<Item> qualityNegativeAfter2ndUpdate = Arrays.stream(app.items).filter(item -> item.quality < 0 ).findAny();

        assertFalse(qualityNegativeAfter2ndUpdate.isPresent());
    }

}
