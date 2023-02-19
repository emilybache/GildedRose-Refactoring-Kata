package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {

            // to avoid to have to check it later on
            if (items[i].name.equals("Sulfuras, Hand of Ragnaros")) {
                continue;
            }

            // first we define what should increase and what should decrease
            if (items[i].name.equals("Backstage passes to a TAFKAL80ETC concert") |
                items[i].name.equals("Aged Brie")) {
                if (items[i].quality < 50) {
                    items[i].quality = items[i].quality + 1;

                    if (items[i].name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                        if (items[i].sellIn < 11 && items[i].quality < 50) {
                            items[i].quality = items[i].quality + 1;
                        }
                        if (items[i].sellIn < 6 && items[i].quality < 50) {
                            items[i].quality = items[i].quality + 1;
                        }
                    }
                }
            } else {
                updateNormalAndConjuredItems(items[i]);
            }

            if (items[i].sellIn <= 0) {

                if (items[i].name.equals("Aged Brie") && items[i].quality < 50) {
                    items[i].quality = items[i].quality + 1;
                }

                if (items[i].name.equals("+5 Dexterity Vest") || items[i].name.equals("Elixir of the Mongoose") ||
                    items[i].name.equals("Conjured Mana Cake")) {
                    updateNormalAndConjuredItems(items[i]);
                }
                if (items[i].name.equals("Backstage passes to a TAFKAL80ETC concert")) {
                    items[i].quality = items[i].quality - items[i].quality;
                }
            }
            items[i].sellIn = items[i].sellIn - 1;
        }
    }

    private void updateNormalAndConjuredItems(Item item) {
        if (item.quality > 0) {
            item.quality = item.quality - 1;
            if (item.name.equals("Conjured Mana Cake") && item.quality > 0) {
                item.quality = item.quality - 1;
            }
        }
    }

}
