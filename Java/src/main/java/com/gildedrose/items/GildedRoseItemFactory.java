package com.gildedrose.items;

import com.gildedrose.Item;

import java.util.regex.Pattern;

import static java.util.regex.Pattern.CASE_INSENSITIVE;

public class GildedRoseItemFactory {

    static final Pattern LEGENDARY_NAME_PATTERN = Pattern.compile("Sulfuras.*", CASE_INSENSITIVE);
    static final Pattern AGING_ITEM = Pattern.compile(".*Brie.*", CASE_INSENSITIVE);
    static final Pattern AGING_HARD_DEGRADATION_ITEM = Pattern.compile("backstage passes.*", CASE_INSENSITIVE);
    static final Pattern CONJURED_ITEM = Pattern.compile("Conjured.*", CASE_INSENSITIVE);

    public static GildedRoseItem create(Item item) {
        if (CONJURED_ITEM.matcher(item.name).matches()) {
            return new StandardGildedRoseItem(item, StandardGildedRoseItem.STANDARD_DEGRADATION * 2);
        } else if (AGING_ITEM.matcher(item.name).matches()) {
            return new AgingGildedRoseItem(item, false);
        } else if (AGING_HARD_DEGRADATION_ITEM.matcher(item.name).matches()) {
            return new AgingGildedRoseItem(item, true);
        } else if (LEGENDARY_NAME_PATTERN.matcher(item.name).matches()) {
            return new LegendaryGildedRoseItem(item);
        } else {
            return new StandardGildedRoseItem(item);
        }
    }

}
