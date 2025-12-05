package com.gildedrose;

public  abstract class BaseItemUpdater implements ItemUpdater {

    public abstract void update(Item item);
    protected void decreaseQuality(Item item) {
        item.quality = Math.max(item.quality - 1, 0);
    }
    protected void decreaseQuality(Item item , int amount) {
        item.quality = Math.max(0, item.quality - amount);
    }
    protected void  increaseQuality(Item item) {
        item.quality = Math.min(item.quality + 1, 50);
    }
    protected void increaseQuality(Item item , int amount) {
        item.quality = Math.min(item.quality + amount, 50);
    }
    protected void decreaseSellIn(Item item) {
       item.sellIn--;
    }

}
