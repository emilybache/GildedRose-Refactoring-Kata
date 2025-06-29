package com.gildedrose.application.backstagepasses;

import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class BackstagePassesRuleTest {

    @Test
    void backStageQualityIncreasesByOneWhenIsGreaterThan10Days() {
        //given
        Item backStage11 = new Item("Backstage passes to a TAFKAL80ETC concert", 11, 2);
        ItemAdapter backstageAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES, backStage11);
        BackstagePassesRule backstagePassesRule = new BackstagePassesRule();

        //when
        backstagePassesRule.processItem(backstageAdapter);

        //then
        assertEquals(10, backstageAdapter.getItem().sellIn);
        assertEquals(3, backstageAdapter.getItem().quality);
    }

    @Test
    void backStageQualityIncreasesByTwoWhenThereAre10DaysOrLess() {
        //given
        Item backStage10 = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 2);
        ItemAdapter backstageAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES, backStage10);
        BackstagePassesRule backstagePassesRule = new BackstagePassesRule();

        //when
        backstagePassesRule.processItem(backstageAdapter);

        //then
        assertEquals(9, backstageAdapter.getItem().sellIn);
        assertEquals(4, backstageAdapter.getItem().quality);
    }

    @Test
    void backStageQualityIncreasesByThreeWhenThereAre5DaysOrLess() {
        //given
        Item backStage5 = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 3);
        ItemAdapter backstageAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES, backStage5);
        BackstagePassesRule backstagePassesRule = new BackstagePassesRule();

        //when
        backstagePassesRule.processItem(backstageAdapter);

        //then
        assertEquals(4, backstageAdapter.getItem().sellIn);
        assertEquals(6, backstageAdapter.getItem().quality);
    }

    @Test
    void backStageQualityDecreasesToZeroWhenIsExpired() {
        //given
        Item backStage = new Item("Backstage passes to a TAFKAL80ETC concert", 0, 3);
        ItemAdapter backstageAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES, backStage);
        BackstagePassesRule backstagePassesRule = new BackstagePassesRule();

        //when
        backstagePassesRule.processItem(backstageAdapter);

        //then
        assertEquals(-1, backstageAdapter.getItem().sellIn);
        assertEquals(0, backstageAdapter.getItem().quality);
    }

    @Test
    void backStageQualityIncreasesNoMoreThan50WhenThereAre5DaysOrLess() {
        //given
        Item backStage5 = new Item("Backstage passes to a TAFKAL80ETC concert", 5, 50);
        ItemAdapter backstageAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES, backStage5);
        BackstagePassesRule backstagePassesRule = new BackstagePassesRule();

        //when
        backstagePassesRule.processItem(backstageAdapter);

        //then
        assertEquals(4, backstageAdapter.getItem().sellIn);
        assertEquals(50, backstageAdapter.getItem().quality);
    }

    @Test
    void backStageQualityIncreasesNoMoreThan50WhenThereAre10DaysOrLess() {
        //given
        Item backStage5 = new Item("Backstage passes to a TAFKAL80ETC concert", 10, 50);
        ItemAdapter backstageAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES, backStage5);
        BackstagePassesRule backstagePassesRule = new BackstagePassesRule();

        //when
        backstagePassesRule.processItem(backstageAdapter);

        //then
        assertEquals(9, backstageAdapter.getItem().sellIn);
        assertEquals(50, backstageAdapter.getItem().quality);
    }

    @Test
    void backStageQualityIncreasesNoMoreThan50WhenIsGreaterThan10Days() {
        //given
        Item backStage5 = new Item("Backstage passes to a TAFKAL80ETC concert", 11, 50);
        ItemAdapter backstageAdapter = new ItemAdapter(ItemType.BACKSTAGE_PASSES, backStage5);
        BackstagePassesRule backstagePassesRule = new BackstagePassesRule();

        //when
        backstagePassesRule.processItem(backstageAdapter);

        //then
        assertEquals(10, backstageAdapter.getItem().sellIn);
        assertEquals(50, backstageAdapter.getItem().quality);
    }
}
