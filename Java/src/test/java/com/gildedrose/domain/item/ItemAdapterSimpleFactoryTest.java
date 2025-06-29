package com.gildedrose.domain.item;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ItemAdapterSimpleFactoryTest {

    @Test
    void shouldCreateAgedBrieItemAdapter(){
        //given
        Item agedBrie = new Item("Aged Brie", 2, 0);
        //when
        ItemAdapter result = ItemAdapterSimpleFactory.createItemAdapter(agedBrie);
        //then
        assertEquals(agedBrie.name, result.getItem().name);
        assertEquals(agedBrie.sellIn, result.getItem().sellIn);
        assertEquals(agedBrie.quality, result.getItem().quality);

    }

    @Test
    void shouldCreateBackstagePassesItemAdapter(){
        //given
        Item backstagePasses = new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20);
        //when
        ItemAdapter result = ItemAdapterSimpleFactory.createItemAdapter(backstagePasses);
        //then
        assertEquals(backstagePasses.name, result.getItem().name);
        assertEquals(backstagePasses.sellIn, result.getItem().sellIn);
        assertEquals(backstagePasses.quality, result.getItem().quality);

    }

    @Test
    void shouldCreateSulfurasItemAdapter(){
        //given
        Item sulfuras = new Item("Sulfuras, Hand of Ragnaros", 0, 80);
        //when
        ItemAdapter result = ItemAdapterSimpleFactory.createItemAdapter(sulfuras);
        //then
        assertEquals(sulfuras.name, result.getItem().name);
        assertEquals(sulfuras.sellIn, result.getItem().sellIn);
        assertEquals(sulfuras.quality, result.getItem().quality);

    }

    @Test
    void shouldCreateStandardItemAdapter(){
        //given
        Item standardItem = new Item("Elixir of the Mongoose", 5, 7);
        //when
        ItemAdapter result = ItemAdapterSimpleFactory.createItemAdapter(standardItem);
        //then
        assertEquals(standardItem.name, result.getItem().name);
        assertEquals(standardItem.sellIn, result.getItem().sellIn);
        assertEquals(standardItem.quality, result.getItem().quality);

    }
}
