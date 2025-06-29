package com.gildedrose.domain.item;

public final class ItemAdapterSimpleFactory {

    static final int SULFURAS_DEFAULT_QUALITY = 80;

    public static ItemAdapter createItemAdapter(Item item) {
        ItemType itemType = ItemType.findByValue(item.name);
         return switch (itemType) {
             case AGEG_BRIE -> createAgedBrie(item);
             case BACKSTAGE_PASSES -> createBackStagePasses(item);
             case SULFURAS -> createSulfuras(item);
             case CONJURED -> createConjured(item);
             default -> createStandardItem(item);
         };
    }

    private static ItemAdapter createAgedBrie(Item item) {
        return new ItemAdapter(ItemType.AGEG_BRIE, item);
    }

    private static ItemAdapter createBackStagePasses(Item item) {
        return new ItemAdapter(ItemType.BACKSTAGE_PASSES, item);
    }

    private static ItemAdapter createSulfuras(Item item) {
        return new ItemAdapter(ItemType.SULFURAS, new Item(item.name, item.sellIn, SULFURAS_DEFAULT_QUALITY));
    }

    private static ItemAdapter createStandardItem(Item item) {
        return new ItemAdapter(ItemType.STANDARD, item);
    }

    private static ItemAdapter createConjured(Item item) {
        return new ItemAdapter(ItemType.CONJURED, item);
    }
}
