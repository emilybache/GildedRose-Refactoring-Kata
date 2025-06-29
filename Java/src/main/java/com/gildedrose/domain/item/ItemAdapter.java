package com.gildedrose.domain.item;

public class ItemAdapter {
    private final Item item;
    private final ItemType itemType;

    public ItemAdapter(ItemType itemType, Item item) {
        this.itemType = itemType;
        this.item = item;
    }

    public Item getItem() {
        return item;
    }

    public ItemType getItemType() {
        return itemType;
    }

    @Override
    public String toString() {
        return item.toString();
    }

    public String toStringFull() { return this.toString() + ", " + itemType.getName(); }
}
