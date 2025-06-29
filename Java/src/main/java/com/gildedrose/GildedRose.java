package com.gildedrose;

import com.gildedrose.core.rule.InventoryRuleEngine;
import com.gildedrose.domain.item.Item;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemAdapterSimpleFactory;

class GildedRose {

    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            ItemAdapter itemAdapter = ItemAdapterSimpleFactory.createItemAdapter(item);
            InventoryRuleEngine.applyUpdateRule(itemAdapter);
        }
    }
}
