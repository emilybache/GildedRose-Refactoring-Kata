package com.gildedrose.behavior.quality;

import com.gildedrose.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class IncreasingQualityBehaviorTest {

    private QualityBehavior qualityBehavior;

    @BeforeEach
    public void setUp() throws Exception {
        qualityBehavior = new IncreasingQualityBehavior();
    }

    @Test
    void increaseQuality() {
        Item item = getItem(10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(11, item.quality);
    }

    @Test
    void increaseAboveTreshold() {
        Item item = getItem(99);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(50, item.quality);
    }

    @Test
    void increaseQualityAtTreshold() {
        Item item = getItem(50);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(50, item.quality);
    }

    @Test
    void increaseNegativeQuality() {
        Item item = getItem(-10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    private Item getItem(int quality) {
        return new Item("SomeItem", 0, quality);
    }
}
