package com.gildedrose;

public class GRItem {

    private static final String AGED_BRIE = "Aged Brie";
    private static final String SULFURAS = "Sulfuras, Hand of Ragnaros";
    private static final int MAX_QUALITY = 50;
    private static final int MIN_QUALITY = 0;

    private final Item item;

    public GRItem(Item item) {
        this.item = item;
    }

    public GRItem(String name, int sellIn, int quality) {
        this.item = new Item(name, sellIn, quality);
    }

    public String getName() {
        return item.name;
    }

    public int getSellIn() {
        return item.sellIn;
    }

    public int getQuality() {
        return item.quality;
    }

    public void updateQuality() {
        if (!item.name.equals(AGED_BRIE)
                && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (item.quality > 0) {
                if (!item.name.equals(SULFURAS)) {
                    item.quality = item.quality - 1;
                }
            }
        } else {
            if (item.quality < MAX_QUALITY) {
                item.quality = item.quality + 1;

                if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                    if (item.sellIn < 11) {
                        if (item.quality < MAX_QUALITY) {
                            item.quality = item.quality + 1;
                        }
                    }

                    if (item.sellIn < 6) {
                        if (item.quality < MAX_QUALITY) {
                            item.quality = item.quality + 1;
                        }
                    }
                }
            }
        }

        if (!item.name.equals(SULFURAS)) {
            item.sellIn = item.sellIn - 1;
        }

        if (item.sellIn < 0) {
            if (!item.name.equals(AGED_BRIE)) {
                if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                    if (item.quality > MIN_QUALITY) {
                        if (!item.name.equals(SULFURAS)) {
                            item.quality = item.quality - 1;
                        }
                    }
                } else {
                    item.quality = 0;
                }
            } else {
                if (item.quality < MAX_QUALITY) {
                    item.quality = item.quality + 1;
                }
            }
        }
    }

    public static void validateItem(Item item) {
        if (item.quality < GRItem.MIN_QUALITY) {
            throw new ItemQualityIsNegativeException(item.name);
        } else if (item.quality > GRItem.MAX_QUALITY && !item.name.equals(SULFURAS)) {
            throw new ItemQualityExceedsMaxValueException(item.name);
        }
    }

    @Override
    public String toString() {
        return "GRItem{" +
                "item=" + item +
                '}';
    }
}
