package com.gildedrose;

class GildedRose {
    Item[] items;
    private String brie = "Aged Brie";
    private String backstagePass = "Backstage passes to a TAFKAL80ETC concert";
    private String sulfuras = "Sulfuras, Hand of Ragnaros";

    public GildedRose(Item[] items) {
        this.items = items;
    }

    public void updateQuality() {
        //for (int i = 0; i < items.length; i++) {
            for (Item item : items){  //switching to for each for better readability and simplicity
            if (!item.name.equals(brie)
                    && !item.name.equals(backstagePass)) {
                if (qualityGreaterThanZero(item)){
                    if (!item.name.equals(sulfuras)) {
                        decreaseQuality(item);
                    }
                }
            } else {
                if(qualityLessThanFifty(item)){
                    increaseQuality(item);

                    if (item.name.equals(backstagePass)) {
                        if (checkSellIn(item, 11) && qualityLessThanFifty(item)){
                                increaseQuality(item);
                        }
                        if (checkSellIn(item, 6) && qualityLessThanFifty(item)){
                                increaseQuality(item);
                        }
                    }
                }
            }

            if (!item.name.equals(sulfuras)) {
                decreaseSellIn(item);
            }

            if (checkSellIn(item, 0)){
                if (!item.name.equals(brie)) {
                    if (!item.name.equals(backstagePass) && qualityGreaterThanZero(item) && !item.name.equals(sulfuras)) {
                                decreaseQuality(item);
                    } else {
                        item.quality = item.quality - item.quality;
                    }
                } else {
                        if(qualityLessThanFifty(item)){
                        increaseQuality(item);
                    }
                }
            }
        }
    }

//Adding methods for repeatable usage
    private boolean qualityGreaterThanZero(Item item){
        return item.quality > 0;
    }

    private boolean qualityLessThanFifty(Item item){
        return item.quality < 50;
    }

    private int increaseQuality(Item item){
        return item.quality++;
    }

    private int decreaseQuality(Item item){
        return item.quality--;
    }

    private boolean checkSellIn (Item item, int numCheck){
        return item.sellIn < numCheck;
    }

    private int decreaseSellIn (Item item){
        return item.sellIn--;
    }
}