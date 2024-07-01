package com.gildedrose;
import com.gildedrose.ItemUpdater;

class GildedRose {
    Item[] items; // Inventory*

    public GildedRose(Item[] items) {
        this.items = items;
    }
    public void updateQuality() {
        // Instantiate updaters for each item type. (Can I instantiate itemupdater only when i need it?)
        ItemUpdater defaultUpdater = new DefaultItemUpdater();
        ItemUpdater agedBrieUpdater = new AgedBrieUpdater();
        ItemUpdater backstagePassUpdater = new BackStagePassUpdater();
        ItemUpdater conjuredItemUpdater = new ConjuredItemUpdater();

        // Iterate through items and call corresponding updater
        for (Item item : items) {
            String name = item.name;
            switch (name) {
                case "Aged Brie":
                    agedBrieUpdater.update(item);
                    break;
                case "Backstage passes to a TAFKAL80ETC concert":
                    backstagePassUpdater.update(item);
                    break;
                case "Sulfuras, Hand of Ragnaros":
                    break;
                case "Conjured":
                    conjuredItemUpdater.update(item);
                    break;
                default:
                    defaultUpdater.update(item);
            }
        }
    }



//    /*
//        Aged Brie increases in quality the older it gets.
//     */
//    private void updateAgedBrie(Item item) {
//        if (item.quality == 50)
//            return;
//        item.quality = item.quality + 1;
//        // Do I have to decrease sellIn?
//    }
//
//
//    private void updateAnyItem(Item item) {
//        if (item.quality <= 0 || item.sellIn <= 0)
//            return;
//        if (item.sellIn > 0)
//            item.quality = item.quality - 1;
//        else
//            item.quality = item.quality - 2;
//
//        item.sellIn = item.sellIn - 1;
//    }
//
//    /*
//        like aged brie, increases in Quality as its SellIn value approaches;
//        Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but
//        Quality drops to 0 after the concert
//     */
//    private void updateBackStageTickets(Item item) {
//        if (item.sellIn <= 0) {
//            item.quality = 0;
//        } else if (item.quality >= 50 && item.sellIn > 0) {
//            item.sellIn--;
//        } else if (item.sellIn < 10 && item.quality > 5) {
//            item.quality = item.quality + 2;
//            item.sellIn--;
//        } else if (item.sellIn < 5) {
//            item.quality = item.quality + 3;
//            item.sellIn--;
//        }
//    }
//
//
//    /*
//        "Conjured" items degrade in Quality twice as fast as normal items
//     */
//        private void updateConjured (Item item){
//            if (item.quality <= 0 || item.sellIn <= 0)
//                return;
//            if (item.sellIn > 0)
//                item.quality = item.quality - 2;
//            else
//                item.quality = item.quality - 4;
//            item.sellIn = item.sellIn - 1;
//        }
    }

