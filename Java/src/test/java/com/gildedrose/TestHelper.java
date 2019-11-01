package com.gildedrose;

public class TestHelper {

    static Item getItem(String name, Integer sellIn, Integer quality){
        return new Item(name, sellIn, quality);
    }
}
