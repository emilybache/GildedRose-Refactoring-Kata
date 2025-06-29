package com.gildedrose.core.rules;

import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class UpdateInventoryTemplateRuleTest {

    @Test
    void processItemCorrectly() {
        //given
        UpdateInventoryTemplateRule testRule = createTestRule(true, true, true);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.STANDARD, new Item("Just a test item", 3, 0));
        //when
        testRule.processItem(itemAdapter);

        //then
        assertEquals(2, itemAdapter.getItem().sellIn);
        assertEquals(0, itemAdapter.getItem().quality);
    }

    @Test
    void processItemCorrectlyWhenIsExpired() {
        //given
        UpdateInventoryTemplateRule testRule = createTestRule(true, true, true);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.STANDARD, new Item("Just a test item", 0, 2));
        //when
        testRule.processItem(itemAdapter);

        //then
        assertEquals(-1, itemAdapter.getItem().sellIn);
        assertEquals(2, itemAdapter.getItem().quality);
    }

    @Test
    void processItemCorrectlyWhenNoChangesAreAllowed() {
        //given
        UpdateInventoryTemplateRule testRule = createTestRule(false, false, false);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.STANDARD, new Item("Just a test item", 10, 2));
        //when
        testRule.processItem(itemAdapter);

        //then
        assertEquals(10, itemAdapter.getItem().sellIn);
        assertEquals(2, itemAdapter.getItem().quality);
    }

    private static UpdateInventoryTemplateRule createTestRule(boolean canSubtractSellIn,
                                                              boolean canIncreaseQuality,
                                                              boolean canDecreaseQuality) {
        return new UpdateInventoryTemplateRule() {
            @Override
            protected boolean canSubtractSellIn(ItemAdapter itemAdapter) {
                return canSubtractSellIn;
            }

            @Override
            protected int getQualityFactor(boolean isExpired, ItemAdapter itemAdapter) {
                return 3;
            }

            @Override
            protected boolean canIncreaseQuality(boolean isExpired, ItemAdapter itemAdapter) {
                return canIncreaseQuality;
            }

            @Override
            protected boolean canDecreaseQuality(boolean isExpired, ItemAdapter itemAdapter) {
                return canDecreaseQuality;
            }
        };
    }
}
