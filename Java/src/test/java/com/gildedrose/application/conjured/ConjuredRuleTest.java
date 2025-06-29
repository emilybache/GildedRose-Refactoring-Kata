package com.gildedrose.application.conjured;

import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ConjuredRuleTest {

    @Test
    void conjuredItemQualityDecreasesByTwo() {
        //given
        Item testItem = new Item("Conjured Mana Cake", 3, 6);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.CONJURED, testItem);
        ConjuredRule conjuredRule = new ConjuredRule();

        //when
        conjuredRule.processItem(itemAdapter);

        //then
        assertEquals(2, itemAdapter.getItem().sellIn);
        assertEquals(4, itemAdapter.getItem().quality);
    }

    @Test
    void conjuredItemQualityDecreasesByTwoWhenExpired() {
        //given
        Item testItem = new Item("Conjured Mana Cake", 0, 6);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.CONJURED, testItem);
        ConjuredRule conjuredRule = new ConjuredRule();

        //when
        conjuredRule.processItem(itemAdapter);

        //then
        assertEquals(-1, itemAdapter.getItem().sellIn);
        assertEquals(4, itemAdapter.getItem().quality);
    }
}
