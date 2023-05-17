package com.gildedrose.service;

import com.gildedrose.common.InvalidQualityException;
import com.gildedrose.domain.Item;

import static com.gildedrose.common.Constants.*;

public class GildedRose {
    public Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() throws InvalidQualityException {
        for (Item item : items) {

            switch (item.getName()) {
                case ITEM_NAME_AGED_BRIE:
                    handleAgedBrie(item);
                    break;
                case ITEM_NAME_BACKSTAGE_SULFURAS:
                    break;
                case ITEM_NAME_BACKSTAGE_PASSES_TO_TAFKAL80ETC:
                    handleBackStageItem(item);
                    break;
                case ITEM_NAME_CONJURED:
                    handleConjuredItem(item);
                    break;
                default:
                    handleOtherItem(item);
            }
        }
    }

    private void validateQuality(Item item, final int min, final int max) throws InvalidQualityException {
        if (item.getQuality() < min) {
            throw new InvalidQualityException("The quality of the item: " + item.getName() + " is less than " + min + "!");
        }
        if (item.getQuality() > 50) {
            throw new InvalidQualityException("The quality of the item: " + item.getName() + " is more than " + max + "!");
        }
    }

    private void handleAgedBrie(Item item) throws InvalidQualityException {
        validateQuality(item, 0, 50);
        item.setQuality(item.getQuality() + 1);
    }

    private void handleBackStageItem(Item item) throws InvalidQualityException {
        validateQuality(item, 0, 50);
        int changeBy = 0;
        if (item.getSellIn() <= 10) {
            changeBy += 2;
        }
        if (item.getSellIn() <= 5) {
            changeBy++;
        }
        if (item.getSellIn() < 0) {
            changeBy = 0;
        }
        item.setQuality(changeBy);
    }

    private void handleConjuredItem(Item item) throws InvalidQualityException {
        validateQuality(item, 0, 80);
        item.setQuality(item.getQuality() - 2);
    }

    private void handleOtherItem(Item item) throws InvalidQualityException {
        validateQuality(item, 0, 50);
        if (item.getSellIn() > 0) {
            item.setQuality(item.getQuality() - 1);
        } else {
            item.setQuality(item.getQuality() - 2);
        }
    }
}
