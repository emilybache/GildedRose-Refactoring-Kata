package com.gildedrose;

public class SellInUpdater {

    //Decrease the sellIn date of non-legendary items with every passing day
    public void updateSellInValue(Item item) {
        if (isNoLegendaryItem(item)) {
            item.sellIn = item.sellIn - 1;
        }
    }

    //Check if the item isn't legendary
    private boolean isNoLegendaryItem(Item item) {
        if (item.name.startsWith("Sulfuras,")) {
            return false;
        } else {
            return true;
        }
    }
}
