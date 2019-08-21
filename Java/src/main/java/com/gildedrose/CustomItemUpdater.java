package com.gildedrose;

public abstract class CustomItemUpdater extends ItemUpdater{
    int getNewQuality(Item item){
        return Math.min(item.quality + getUpdateValue(item), HIGHEST_QUALITY);
    }
}
