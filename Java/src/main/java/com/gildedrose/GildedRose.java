package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (int i = 0; i < items.length; i++) {

            // to avoid to have to check it later on
            if (items[i].getName().equals("Sulfuras, Hand of Ragnaros")) {
                continue;
            }

            // first we define what should increase and what should decrease
            if (items[i].getName().equals("Backstage passes to a TAFKAL80ETC concert") |
                items[i].getName().equals("Aged Brie")) {
                if (items[i].getQuality() < 50) {
                    items[i].setQuality(items[i].getQuality() + 1);

                    if (items[i].getName().equals("Backstage passes to a TAFKAL80ETC concert")) {
                        if (items[i].getSellIn() < 11 && items[i].getQuality() < 50) {
                            items[i].setQuality(items[i].getQuality() + 1);
                        }
                        if (items[i].getSellIn() < 6 && items[i].getQuality() < 50) {
                            items[i].setQuality(items[i].getQuality() + 1);
                        }
                    }
                }
            } else {
                updateNormalAndConjuredItems(items[i]);
            }

            if (items[i].getSellIn() <= 0) {

                if (items[i].getName().equals("Aged Brie") && items[i].getQuality() < 50) {
                    items[i].setQuality(items[i].getQuality() + 1);
                }

                if (items[i].getName().equals("+5 Dexterity Vest") || items[i].getName().equals("Elixir of the Mongoose") ||
                    items[i].getName().equals("Conjured Mana Cake")) {
                    updateNormalAndConjuredItems(items[i]);
                }
                if (items[i].getName().equals("Backstage passes to a TAFKAL80ETC concert")) {
                    items[i].setQuality(0);
                }
            }
            items[i].setSellIn(items[i].getSellIn() - 1);
        }
    }

    private void updateNormalAndConjuredItems(Item item) {
        if (item.getQuality() > 0) {
            item.setQuality(item.getQuality() - 1);
            if (item.getName().equals("Conjured Mana Cake") && item.getQuality() > 0) {
                item.setQuality(item.getQuality() - 1);
            }
        }
    }

}
