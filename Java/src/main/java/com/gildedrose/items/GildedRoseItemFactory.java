package com.gildedrose.items;

import com.gildedrose.Item;

import java.util.regex.Pattern;

import static java.util.regex.Pattern.CASE_INSENSITIVE;

public class GildedRoseItemFactory {

    static final Pattern LEGENDARY_NAME_PATTERN = Pattern.compile("Sulfuras.*", CASE_INSENSITIVE);
    static final Pattern AGING_ITEM = Pattern.compile("", CASE_INSENSITIVE);
    static final Pattern CONJURED_ITEM = Pattern.compile("Conjured.*", CASE_INSENSITIVE);

    public static GildedRoseItem create(Item item) {
        if (AGING_ITEM.matcher(item.name).matches()
            || CONJURED_ITEM.matcher(item.name).matches()) {
            return new NonStandardGildedRoseItem(item);
        }  else if (LEGENDARY_NAME_PATTERN.matcher(item.name).matches()) {
            return new LegendaryGildedRoseItem(item);
        } else {
            return new StandardGildedRoseItem(item);
        }
    }

}
