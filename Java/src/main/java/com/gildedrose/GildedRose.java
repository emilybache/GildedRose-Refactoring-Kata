package com.gildedrose;

class GildedRose {
    Item[] items;

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        for (Item item : items) {
            boolean isAgedBrie = item.getName().equals("Aged Brie");
            boolean isSulfuras = item.getName().equals("Sulfuras, Hand of Ragnaros");
            boolean passesToTafkalConcert = item.getName().equals("Backstage passes to a TAFKAL80ETC concert");

            if (!isAgedBrie && !passesToTafkalConcert) {
                if (item.quality > 0 && !isSulfuras) {
                    deductOneFromQuality(item);
                }
            } else if(item.quality < 50) {
                addOneToQuality(item);

                if (passesToTafkalConcert) {
                    if (item.sellIn < 11 && item.quality < 50) {
                        addOneToQuality(item);
                    }

                    if (item.sellIn < 6 && item.quality < 50) {
                        addOneToQuality(item);
                    }
                }
            }

            if (!isSulfuras) {
                deductSellIn(item);
            }

            if (item.sellIn < 0) {
                if (!isAgedBrie) {
                    if (!passesToTafkalConcert) {
                        if (item.quality > 0 && !isSulfuras) {
                            deductOneFromQuality(item);
                        }
                    } else {
                        setQualityToZero(item);
                    }
                } else if ((item.quality < 50)) {
                    addOneToQuality(item);
                }
            }
        }
    }

    public void setQualityToZero(Item item) {
        item.quality = 0;
    }

  public void deductSellIn(Item item) {
        item.sellIn--;
  }

    public void addOneToQuality(Item item) {
        item.quality++;
    }

    public void deductOneFromQuality(Item item) {
        item.quality--;
    }
}
