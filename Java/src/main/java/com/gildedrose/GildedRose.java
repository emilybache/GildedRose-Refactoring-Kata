package com.gildedrose;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

class GildedRose {
    Item[] items;
    private final List<ItemWrapper> itemWrappers;

    public GildedRose(Item[] items) {
        this.items = items;
        this.itemWrappers = new ArrayList<>(items.length);
        Arrays.stream(items).forEach(item -> itemWrappers.add(new ItemWrapper(item)));
    }

    public void updateQuality() {
        itemWrappers.forEach(ItemWrapper::updateQuality);
    }
}
