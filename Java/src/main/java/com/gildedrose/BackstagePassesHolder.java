package com.gildedrose;

public class BackstagePassesHolder extends ItemHolder {

    public BackstagePassesHolder(Item item) {
        super(item);
    }
    public void updateQuality() {
        if (this.item.sellIn > 10) {
            this.item.quality += 1;
        } else if (this.item.sellIn > 5) {
            this.item.quality += 2;
        } else if (this.item.sellIn > 0) {
            this.item.quality += 3;
        } else if (this.item.sellIn <= 0) {
            this.item.quality = 0;
        }
        this.item.quality = Math.min(this.item.quality, 50);
    }

    public void updateSellIn() {
      this.item.sellIn -= 1;
    }
}
