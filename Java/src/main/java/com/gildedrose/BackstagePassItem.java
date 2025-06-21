package com.gildedrose;

public class BackstagePassItem extends UpdatableItem {
    public BackstagePassItem(int sellIn, int quality) {
        super("Backstage passes to a TAFKAL80ETC concert", sellIn, quality);
    }
    @Override
    public void update() {
        sellIn--;
        if (sellIn < 0) {
            quality = 0;
        } else if (sellIn < 5) {
            quality = Math.min(quality + 3, 50);
        } else if (sellIn < 10) {
            quality = Math.min(quality + 2, 50);
        } else if (quality < 50) {
            quality++;
        }
    }
} 