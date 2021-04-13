package com.gildedrose.behavior.quality;

import com.gildedrose.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ImmutableQualityBehaviorTest {

    private QualityBehavior qualityBehavior;

    @BeforeEach
    public void setUp() throws Exception {
        qualityBehavior = new ImmutableQualityBehavior();
    }

    @Test
    void immutableQuality() {
        Item item = getItem(10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(10, item.quality);
    }

    @Test
    void immutableQualityZero() {
        Item item = getItem(0);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(0, item.quality);
    }

    @Test
    void immutableQualityNegative() {
        Item item = getItem(-10);
        qualityBehavior.processQualityUpdate(item);

        assertEquals(-10, item.quality);
    }

    private Item getItem(int quality) {
        return new Item("SomeItem", 0, quality);
    }
}
