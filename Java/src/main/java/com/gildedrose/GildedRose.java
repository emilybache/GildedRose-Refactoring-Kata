package com.gildedrose;

import com.gildedrose.rule.AgedBrieRule;
import com.gildedrose.rule.BackstageRule;
import com.gildedrose.rule.ConjuredRule;
import com.gildedrose.rule.NormalRule;
import com.gildedrose.rule.Rule;
import com.gildedrose.rule.SulfurasRule;

import java.util.ArrayList;
import java.util.List;

class GildedRose {
    Item[] items;
    List<Rule> rules;

    public GildedRose(Item[] items) {
        this.items = items;
        rules = new ArrayList<>();
        rules.add(new NormalRule());
        rules.add(new AgedBrieRule());
        rules.add(new BackstageRule());
        rules.add(new SulfurasRule());
        rules.add(new ConjuredRule());

    }

    public void updateQuality() {
        for (Item item : items) {
            rules.stream()
                .filter(rule -> rule.match(item))
                .forEach(rule -> rule.apply(item));
        }
    }
}


