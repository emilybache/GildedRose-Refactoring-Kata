package com.gildedrose.application.sulfuras;

import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SulfurasRuleTest {

    @Test
    void sulfurasNeverChanges() {
        //given
        Item sulfuras = new Item("Sulfuras, Hand of Ragnaros", -1, 80);
        ItemAdapter sulfurasAdapter = new ItemAdapter(ItemType.SULFURAS, sulfuras);
        SulfurasRule sulfurasRule = new SulfurasRule();

        //when
        sulfurasRule.processItem(sulfurasAdapter);

        //then
        assertEquals(-1, sulfurasAdapter.getItem().sellIn);
        assertEquals(80, sulfurasAdapter.getItem().quality);
    }

    @Test
    void sulfurasQualityLevelIsAlways80() {
        //given
        Item sulfuras = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
        ItemAdapter sulfurasAdapter = new ItemAdapter(ItemType.SULFURAS, sulfuras);
        SulfurasRule sulfurasRule = new SulfurasRule();

        //when
        sulfurasRule.processItem(sulfurasAdapter);

        //then
        assertEquals(80, sulfurasAdapter.getItem().quality);
    }
}
