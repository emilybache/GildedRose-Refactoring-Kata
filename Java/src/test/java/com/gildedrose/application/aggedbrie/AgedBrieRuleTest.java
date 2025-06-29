package com.gildedrose.application.aggedbrie;

import com.gildedrose.application.agedbrie.AgedBrieRule;
import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class AgedBrieRuleTest {

    @Test
    void agedBrieQualityIncreasesByOne() {
        //given
        Item agedBrie = new Item("Aged Brie", 5, 2);
        ItemAdapter agedBrieAdapter = new ItemAdapter(ItemType.AGEG_BRIE, agedBrie);
        AgedBrieRule agedBrieRule = new AgedBrieRule();

        //when
        agedBrieRule.processItem(agedBrieAdapter);

        //then
        assertEquals(4, agedBrieAdapter.getItem().sellIn);
        assertEquals(3, agedBrieAdapter.getItem().quality);
    }

    @Test
    void agedBrieQualityIncreasesByTwoWhenExpired() {
        //given
        Item agedBrie = new Item("Aged Brie", 0, 2);
        ItemAdapter agedBrieAdapter = new ItemAdapter(ItemType.AGEG_BRIE, agedBrie);
        AgedBrieRule agedBrieRule = new AgedBrieRule();

        //when
        agedBrieRule.processItem(agedBrieAdapter);

        //then
        assertEquals(-1, agedBrieAdapter.getItem().sellIn);
        assertEquals(4, agedBrieAdapter.getItem().quality);
    }

    @Test
    void agedBrieQualityIncreasesNoMoreThan50() {
        //given
        Item agedBrie = new Item("Aged Brie", 5, 50);
        ItemAdapter agedBrieAdapter = new ItemAdapter(ItemType.AGEG_BRIE, agedBrie);
        AgedBrieRule agedBrieRule = new AgedBrieRule();

        //when
        agedBrieRule.processItem(agedBrieAdapter);

        //then
        assertEquals(4, agedBrieAdapter.getItem().sellIn);
        assertEquals(50, agedBrieAdapter.getItem().quality);
    }

    @Test
    void agedBrieQualityIncreasesNoMoreThan50WhenIsExpired() {
        //given
        Item agedBrie = new Item("Aged Brie", 0, 50);
        ItemAdapter agedBrieAdapter = new ItemAdapter(ItemType.AGEG_BRIE, agedBrie);
        AgedBrieRule agedBrieRule = new AgedBrieRule();

        //when
        agedBrieRule.processItem(agedBrieAdapter);

        //then
        assertEquals(-1, agedBrieAdapter.getItem().sellIn);
        assertEquals(50, agedBrieAdapter.getItem().quality);
    }
}
