package com.gildedrose;

abstract class LegendaryItemUpdater extends ItemUpdater{
    static int HIGHEST_QUALITY = 80;

    int getNewQuality(final Item item){
        return Math.min(item.quality + getUpdateValue(item), HIGHEST_QUALITY);
    }
}
