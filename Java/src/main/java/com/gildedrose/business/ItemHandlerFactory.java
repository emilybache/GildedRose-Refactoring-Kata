package com.gildedrose.business;

public class ItemHandlerFactory {

    public ItemHandler createItemHandler(String itemName) {

        if (ItemEnum.AGED_BRIE.getValue().equals(itemName)) {
            return new AgedBrieItemHandler();

        } else if (ItemEnum.BACKSTAGE_PASSES.getValue().equals(itemName)) {
            return new BackstagePassesItemHandler();

        } else if (ItemEnum.CONJURED_MANA_CAKE.getValue().equals(itemName)) {
            return new ConjuredItemHandler();

        } else {
            return new RegularItemHandler();
        }
    }
}

