package com.gildedrose;

import com.gildedrose.itemsorts.QualityItem;

import java.util.ArrayList;
import java.util.List;

public class GildedRose {
    private final List<QualityItem> items;
    private final QualityItemHandler qualityItemHandler;

    public GildedRose(List<QualityItem> items, QualityItemHandler qualityItemHandler) {
        this.items = items;
        this.qualityItemHandler = qualityItemHandler;
    }

    public List<QualityItem> getItems() {
        return new ArrayList<>(items);
    }

    public void updateQuality() {
        items.replaceAll(qualityItemHandler::processOneItem);
    }
}