package com.gildedrose;

class GildedRose {
    Item[] items;

    String itemName;
    int itemQuality;
    int itemSellIn;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {
            itemName = items[i].name;
            itemQuality = items[i].quality;
            itemSellIn = items[i].sellIn;

            if (!itemIsAgedBrie(itemName)
                    && !itemIsBackstagePasses(itemName)) {
                if (itemQuality > 0) {
                    if (!itemIsSulfuras(itemName)) {
                        decreaseByOne(itemQuality);
                    }
                }
            } else {
                if (itemQuality < 50) {
                    itemQuality = itemQuality + 1;

                    if (itemIsBackstagePasses(itemName)) {
                        if (itemSellIn < 11) {
                            if (itemQuality < 50) {
                                increaseByOne(itemQuality);
                            }
                        }

                        if (itemSellIn < 6) {
                            if (itemQuality < 50) {
                                increaseByOne(itemQuality);
                            }
                        }
                    }
                }
            }

            if (!itemIsSulfuras(itemName)) {
                decreaseByOne(itemSellIn);
            }

            if (itemSellIn < 0) {
                if (!itemIsAgedBrie(itemName)) {
                    if (!itemIsBackstagePasses(itemName)) {
                        if (itemQuality > 0) {
                            if (!itemIsSulfuras(itemName)) {
                                decreaseByOne(itemQuality);
                            }
                        }
                    } else {
                        decreaseByOne(itemQuality);
                    }
                } else {
                    if (itemQuality < 50) {
                        increaseByOne(itemQuality);
                    }
                }
            }
        }
    }

    public void increaseByOne (int itemValue) {
        itemValue++;
    }

    public void decreaseByOne (int itemValue) {
        itemValue--;
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
}
