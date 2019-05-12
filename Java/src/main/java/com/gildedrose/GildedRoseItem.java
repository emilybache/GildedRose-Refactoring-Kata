package com.gildedrose;

/**
 * Class which update quality for all items
 */
class GildedRoseItem  {
    Item[] items;
    private ItemFactory itemFactory;

    public GildedRoseItem(Item[] items) {
        this.items = items;
        itemFactory = new ItemFactory();
    }

    /**
     * Update quality and number of days left to sell
     */
    public void updateQuality() {
        for (Item item : items) {
            ItemInterface typeItem = itemFactory.createItemType(item);
            typeItem.updateNumberOfdayToSellRemaining();
            typeItem.updateQuality();
        }
    }
}
