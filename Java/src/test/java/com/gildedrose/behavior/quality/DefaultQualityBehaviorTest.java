package com.gildedrose.behavior.quality;

import com.gildedrose.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DefaultQualityBehaviorTest {

    private QualityBehavior qualityBehavior;

    @BeforeEach
    public void setUp() throws Exception {
        qualityBehavior = new DefaultQualityBehavior();
    }

    @Test
    void decreaseQuality() {
        Item item = getItem(5,10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(9, item.quality);
    }

    @Test
    void decreaseNegativeQuality() {
        Item item = getItem(5,-10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    @Test
    void decreaseQualityZero() {
        Item item = getItem(5,0);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    @Test
    void decreaseQualityFaster() {
        Item item = getItem(0,4);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(2, item.quality);
    }

    @Test
    void decreaseQualityFasterNegativeSellIn() {
        Item item = getItem(-1,4);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(2, item.quality);
    }

    @Test
    void decreaseQualityFasterRespectLowerLimit() {
        Item item = getItem(0,1);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    private Item getItem(int sellIn, int quality) {
        return new Item("SomeItem", sellIn, quality);
    }
}
