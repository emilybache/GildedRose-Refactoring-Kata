package com.gildedrose.core;

import com.gildedrose.application.agedbrie.AgedBrieRule;
import com.gildedrose.application.backstagepasses.BackstagePassesRule;
import com.gildedrose.application.sulfuras.SulfurasRule;
import com.gildedrose.core.rules.UpdateInventoryTemplateRule;
import com.gildedrose.domain.item.ItemAdapter;
import com.gildedrose.domain.item.ItemType;

import java.util.HashMap;
import java.util.Map;

public class InventoryRuleEngine {

    private static final Map<ItemType, UpdateInventoryTemplateRule> updateInventoryRules = new HashMap<>(){{
        put(ItemType.AGEG_BRIE, new AgedBrieRule());
        put(ItemType.SULFURAS, new SulfurasRule());
        put(ItemType.BACKSTAGE_PASSES, new BackstagePassesRule());
    }};

    public static void applyUpdateRule(ItemAdapter itemAdapter) {
        if (null != updateInventoryRules.get(itemAdapter.getItemType())) {
            updateInventoryRules.get(itemAdapter.getItemType()).processItem(itemAdapter);
        }
    }
}
