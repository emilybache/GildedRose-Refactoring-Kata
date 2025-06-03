package com.gildedrose;

class SulfurasItem extends UpdatableItem {
    public static final int SULFURAS_CONSTANT_QUALITY = 80;

    public SulfurasItem(Item item) {
        super(item);
        item.quality = SULFURAS_CONSTANT_QUALITY;
    }

    @Override
    public void update() {}
}

