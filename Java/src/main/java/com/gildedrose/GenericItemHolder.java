package com.gildedrose;

public class GenericItemHolder extends ItemHolder {

    public GenericItemHolder(Item item) {
        super(item);
    }
    public void updateQuality() {
        if (this.item.sellIn > 0) {
            this.item.quality -= 1;
        } else {
            this.item.quality -= 2;
        }
        this.item.quality = Math.max(this.item.quality, 0);
    }

    public void updateSellIn() {
      this.item.sellIn -= 1;
    }
}
