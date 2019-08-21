package com.gildedrose;

abstract class LegendaryItemUpdater extends ItemUpdater{
    static int HIGHEST_QUALITY = 80;

    LegendaryItemUpdater() {
    }

    int getNewQuality(){
        return Math.min(item.quality + getUpdateValue(), HIGHEST_QUALITY);
    }
}
