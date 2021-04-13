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
        Item item = getItem(10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(9, item.quality);
    }

    @Test
    void decreaseNegativeQuality() {
        Item item = getItem(-10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    @Test
    void decreaseQualityZero() {
        Item item = getItem(0);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    private Item getItem(int quality) {
        return new Item("SomeItem", 0, quality);
    }
}
