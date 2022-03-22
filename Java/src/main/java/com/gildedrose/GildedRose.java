package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            String itemName = items[i].name;
            int itemQuality = items[i].quality;
            int itemSellIn = items[i].sellIn;

            adjustQualityNotation(itemName, itemQuality, itemSellIn);
            lowerSellInNotation(itemName, itemSellIn);

            if (itemSellIn < 0) {
                if (isDecreasableItem(itemName, itemQuality)) {
                    decreaseByOne(itemQuality);
                } else if (itemQuality < 50) {
                        increaseByOne(itemQuality);
                }
            }
        }
    }

    public void adjustQualityNotation(String itemName, int itemQuality, int itemSellIn) {
        if (isDecreasableItem(itemName, itemQuality)) {
            decreaseByOne(itemQuality);
        } else if (qualityLowerThanFifty(itemQuality)) {
                increaseByOne(itemQuality);
                backstagePassesExtraQualityCheck(itemName, itemQuality, itemSellIn);
        }
    }

    public void backstagePassesExtraQualityCheck(String itemName, int itemQuality, int itemSellIn) {
        if (itemIsBackstagePasses(itemName) && qualityLowerThanFifty(itemQuality)) {
            if (itemSellIn < 11) {
                increaseByOne(itemQuality);
            }

            if (itemSellIn < 6) {
                increaseByOne(itemQuality);
            }
        }
    }

    public void lowerSellInNotation(String itemName, int itemSellIn) {
        if (!itemIsSulfuras(itemName)) {
            decreaseByOne(itemSellIn);
        }
    }

    public int increaseByOne (int incomingValue) {
        return incomingValue += 1;
    }

    public int decreaseByOne (int incomingValue) {
        return incomingValue -= 1;
    }



    public boolean itemIsSulfuras(String itemName) {
        return ProjectConstants.SULFURAS.equals(itemName) ? true : false;
    }

    public boolean itemIsBackstagePasses(String itemName) {
        return ProjectConstants.BACKSTAGE_PASSES.equals(itemName) ? true : false;
    }

    public boolean itemIsAgedBrie(String itemName) {
        return ProjectConstants.AGED_BRIE.equals(itemName) ? true : false;
    }

    public boolean qualityHigherThanZero(int itemQuality) {
        return itemQuality > 0;
    }

    public boolean qualityLowerThanFifty(int itemQuality) {
        return itemQuality < 50;
    }

    public boolean isDecreasableItem(String itemName, int itemQuality) {
        return qualityHigherThanZero(itemQuality) &&
            !itemIsAgedBrie(itemName) &&
            !itemIsBackstagePasses(itemName) &&
            !itemIsSulfuras(itemName);
    }
}
