package com.gildedrose;

abstract class CustomItemUpdater extends ItemUpdater{

    CustomItemUpdater() {
    }

    int getNewQuality(){
        return Math.min(item.quality + getUpdateValue(), HIGHEST_QUALITY);
    }
}
