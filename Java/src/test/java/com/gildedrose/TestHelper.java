package com.gildedrose;

import com.gildedrose.item.Item;

public class TestHelper {

    static Item getItem(String name, Integer sellIn, Integer quality){
        return new Item(name, sellIn, quality);
    }
}
