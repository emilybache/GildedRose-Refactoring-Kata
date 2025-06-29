package com.gildedrose.application.standard;

import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class StandardItemRuleTest {

    @Test
    void whenSellInDateIsPassed_qualityDegradesTwiceAsFast() {
        //given
        Item testItem = new Item("standard item", 0, 3);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.STANDARD, testItem);
        StandardItemRule standardItemRule = new StandardItemRule();

        //when
        standardItemRule.processItem(itemAdapter);

        //then
        assertEquals(-1, itemAdapter.getItem().sellIn);
        assertEquals(1, itemAdapter.getItem().quality);
    }

    @Test
    void qualityOfAnItemIsNeverNegative() {
        //given
        Item testItem = new Item("standard item", 0, 0);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.STANDARD, testItem);
        StandardItemRule standardItemRule = new StandardItemRule();

        //when
        standardItemRule.processItem(itemAdapter);

        //then
        assertEquals(-1, itemAdapter.getItem().sellIn);
        assertEquals(0, itemAdapter.getItem().quality);
    }

    @Test
    void standardItemQualityDecreasesByOne() {
        //given
        Item testItem = new Item("standard item", 5, 2);
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.STANDARD, testItem);
        StandardItemRule standardItemRule = new StandardItemRule();

        //when
        standardItemRule.processItem(itemAdapter);

        //then
        assertEquals(4, itemAdapter.getItem().sellIn);
        assertEquals(1, itemAdapter.getItem().quality);
    }
}
