package com.gildedrose.core.rule;

import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class InventoryRuleEngineTest {

    @Test
    void shouldApplyAgedBrieRule() {
        //given
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.AGEG_BRIE, new Item("Aged Brie", 3, 0));

        //when
        InventoryRuleEngine.applyUpdateRule(itemAdapter);

        //then
        assertEquals(2, itemAdapter.getItem().sellIn);
        assertEquals(1, itemAdapter.getItem().quality);
    }

    @Test
    void shouldApplySulfurasRule() {
        //given
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.SULFURAS,
            new Item("Sulfuras, Hand of Ragnaros", 3, 80));

        //when
        InventoryRuleEngine.applyUpdateRule(itemAdapter);

        //then
        assertEquals(3, itemAdapter.getItem().sellIn);
        assertEquals(80, itemAdapter.getItem().quality);
    }

    @Test
    void shouldApplyBackstagePassesRule() {
        //given
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES,
            new Item("Backstage passes to a TAFKAL80ETC concert", 3, 10));

        //when
        InventoryRuleEngine.applyUpdateRule(itemAdapter);

        //then
        assertEquals(2, itemAdapter.getItem().sellIn);
        assertEquals(13, itemAdapter.getItem().quality);
    }

    @Test
    void shouldApplyStandardItemRule() {
        //given
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.STANDARD,
            new Item("Elixir of the Mongoose", 5, 7));

        //when
        InventoryRuleEngine.applyUpdateRule(itemAdapter);

        //then
        assertEquals(4, itemAdapter.getItem().sellIn);
        assertEquals(6, itemAdapter.getItem().quality);
    }

    @Test
    void shouldApplyConjuredRule() {
        //given
        ItemAdapter itemAdapter = new ItemAdapter(ItemType.CONJURED,
            new Item("Conjured Mana Cake", 3, 6));

        //when
        InventoryRuleEngine.applyUpdateRule(itemAdapter);

        //then
        assertEquals(2, itemAdapter.getItem().sellIn);
        assertEquals(4, itemAdapter.getItem().quality);
    }
}
