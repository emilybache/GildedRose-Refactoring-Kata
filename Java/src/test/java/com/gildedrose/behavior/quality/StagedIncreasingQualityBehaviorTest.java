package com.gildedrose.behavior.quality;

import com.gildedrose.Item;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class StagedIncreasingQualityBehaviorTest {

    private QualityBehavior stagedQualityBehavior;

    private QualityStage stage1 = QualityStage.of(10, 2);
    private QualityStage stage2 = QualityStage.of(5, 3);

    @BeforeEach
    public void setUp() throws Exception {
        stagedQualityBehavior = StagedIncreasingQualityBehavior.withStages(new ArrayList<>(Arrays.asList(stage1, stage2)), 1);
    }

    @Test
    void stagedIncrease() {
        Item item = getItem(11, 1);
        stagedQualityBehavior.processQualityUpdate(item);
        assertEquals(2, item.quality);

        item.sellIn = 10;
        stagedQualityBehavior.processQualityUpdate(item);
        assertEquals(4, item.quality);

        item.sellIn = 6;
        stagedQualityBehavior.processQualityUpdate(item);
        assertEquals(6, item.quality);

        item.sellIn = 5;
        stagedQualityBehavior.processQualityUpdate(item);
        assertEquals(9, item.quality);

        item.sellIn = 1;
        stagedQualityBehavior.processQualityUpdate(item);
        assertEquals(12, item.quality);

        item.sellIn = 0;
        stagedQualityBehavior.processQualityUpdate(item);
        assertEquals(0, item.quality);

        item.sellIn = -10;
        item.quality = 10;
        stagedQualityBehavior.processQualityUpdate(item);
        assertEquals(0, item.quality);
    }

    private Item getItem(int sellIn, int quality) {
        return new Item("SomeItem", sellIn, quality);
    }
}
