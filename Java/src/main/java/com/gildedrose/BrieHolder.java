package com.gildedrose;

public class BrieHolder extends ItemHolder {

    public BrieHolder(Item item) {
        super(item);
    }
    public void updateQuality() {
        if (this.item.quality < 50) {
            this.item.quality += 1;
        }
    }

    public void updateSellIn() {
      this.item.sellIn -= 1;
    }
}
