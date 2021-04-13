package com.gildedrose.behavior.quality;

import com.gildedrose.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DecreasingQualityBehaviorTest {

    private QualityBehavior qualityBehavior;
    private QualityBehavior fastDecreasingQualityBehavior;

    @BeforeEach
    public void setUp() throws Exception {
        qualityBehavior = DecreasingQualityBehavior.newInstance();
        fastDecreasingQualityBehavior = DecreasingQualityBehavior.newInstance(2,4);
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

    @Test
    void fastDecreaseQuality() {
        Item item = getItem(5,10);
        fastDecreasingQualityBehavior.processQualityUpdate(item);

        assertEquals(8, item.quality);
    }

    @Test
    void fastDecreaseNegativeQuality() {
        Item item = getItem(5,-10);
        fastDecreasingQualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    @Test
    void fastDecreaseQualityZero() {
        Item item = getItem(5,0);
        fastDecreasingQualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    @Test
    void fastDecreaseQualityFaster() {
        Item item = getItem(0,8);
        fastDecreasingQualityBehavior.processQualityUpdate(item);

        assertEquals(4, item.quality);
    }

    @Test
    void fastDecreaseQualityFasterNegativeSellIn() {
        Item item = getItem(-1,8);
        fastDecreasingQualityBehavior.processQualityUpdate(item);

        assertEquals(4, item.quality);
    }

    @Test
    void fastDecreaseQualityFasterRespectLowerLimit() {
        Item item = getItem(0,1);
        fastDecreasingQualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    private Item getItem(int sellIn, int quality) {
        return new Item("SomeItem", sellIn, quality);
    }
}
