package com.gildedrose;

import org.junit.jupiter.api.Test;

import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class GildedRoseTest {
    private final int days = 2;

    @Test
    void shouldDecreaseQualityEachUpdate() {
        assertCalculatedValues(getDecreaseQualityItems(),
                new GildedRose(getDecreaseQualityItems()), CalcQuality.DECREASE);
    }

    @Test
    void shouldIncreaseQualityEachUpdate() {
        assertCalculatedValues(getIncreaseQualityItems(),
                new GildedRose(getIncreaseQualityItems()), CalcQuality.INCREASE);
    }

    @Test
    void shouldRetainPropsEachUpdate() {
        assertCalculatedValues(getRetainPropsItems(),
                new GildedRose(getRetainPropsItems()), CalcQuality.NOTHING);
    }

    private Item[] getRetainPropsItems() {
        return new Item[] {
                new Item("Sulfuras, Hand of Ragnaros", 0, 80),
                new Item("Sulfuras, Hand of Ragnaros", -1, 80)
        };
    }

    private Item[] getIncreaseQualityItems() {
        return new Item[] {
                new Item("Aged Brie", 2, 0),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
                new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)
        };
    }

    private Item[] getDecreaseQualityItems() {
        return new Item[] {
                new Item("+5 Dexterity Vest", 10, 20),
                new Item("Elixir of the Mongoose", 5, 7)
        };
    }

    private void assertCalculatedValues(Item[] originItems, GildedRose app, CalcQuality calcQuality) {
        IntStream.range(1, this.days).forEach(day -> {
            app.updateQuality();
            IntStream.range(0, originItems.length).forEach(index -> {
                assertEquals(originItems[index].name, app.items[index].name);
                int sellInCalculated = getSellInCalculated(originItems[index].sellIn, calcQuality, day);
                assertEquals(sellInCalculated, app.items[index].sellIn);
                int quantityCalculated = getQualityCalculated(originItems[index].quality, calcQuality, day);
                assertEquals(quantityCalculated, app.items[index].quality);
            });
        });
    }

    private int getSellInCalculated(int sellIn, CalcQuality calcQuality, int day) {
        if(!calcQuality.equals(CalcQuality.NOTHING)) {
            return sellIn - day;
        }
        return sellIn;
    }

    private int getQualityCalculated(int quality, CalcQuality calcQuality, int day) {
        if(calcQuality.equals(CalcQuality.DECREASE)) {
            return quality - day;
        }
        if(calcQuality.equals(CalcQuality.INCREASE)) {
            return quality + day;
        }
        return quality;
    }
}

enum CalcQuality {
    INCREASE, DECREASE, NOTHING
}
