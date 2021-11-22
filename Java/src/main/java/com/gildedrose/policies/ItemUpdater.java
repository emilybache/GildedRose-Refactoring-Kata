package com.gildedrose.policies;

import com.gildedrose.Item;

public class ItemUpdater {

    public static void updateItem(Item item){
        UpdatePolicesFactory.getUpdatePolicy(item).updateItem();
    }
}
