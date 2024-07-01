package com.gildedrose;

public interface ItemUpdater {
    void update(Item item);
}

class DefaultItemUpdater implements ItemUpdater {
    @Override
    public void update(Item item) {
        decreaseQuality(item);
        item.sellIn--;
        if (item.sellIn < 0)
            decreaseQuality(item);
    }

    protected void decreaseQuality(Item item) {
        if (item.quality > 0)
            item.quality--;
    }
}

class AgedBrieUpdater implements ItemUpdater {
    @Override
    public void update(Item item) {
        if (item.quality < 50)
            item.quality++;
        item.sellIn--;
    }
}

class BackStagePassUpdater implements ItemUpdater {
    @Override
    public void update(Item item) {
        if (item.quality < 50)
            item.quality++;
        if (item.sellIn <= 10 && item.quality < 50)
            item.quality++;
        if (item.sellIn <= 5 && item.quality < 50)
            item.quality++;
        item.sellIn--;
        if (item.sellIn < 0)
            item.quality = 0;
    }
}

class ConjuredItemUpdater extends DefaultItemUpdater {
    @Override
    protected void decreaseQuality(Item item) {
        if (item.quality > 0)
            item.quality -= 2;
    }
}
