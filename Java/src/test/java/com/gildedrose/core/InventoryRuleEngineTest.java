package com.gildedrose.core;

import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class InventoryRuleEngineTest {

    @Test
    void shouldProcessAgedBrieRule() {
        //given
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.AGEG_BRIE, new Item("Aged Brie", 3, 0));

        //when
        InventoryRuleEngine.applyUpdateRule(itemAdapter);

        //then
        assertEquals(2, itemAdapter.getItem().sellIn);
        assertEquals(1, itemAdapter.getItem().quality);
    }
}
