package com.gildedrose;

public class DefaultCategory extends Category {

    public DefaultCategory() {
        super(null);
        withQualityUpdater(new StandardQualityDegrader());
    }
}
