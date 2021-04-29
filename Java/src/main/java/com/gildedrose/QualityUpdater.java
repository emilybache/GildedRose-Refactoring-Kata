package com.gildedrose;

public class QualityUpdater {

    //Find items with exceptional quality rules
    public void updateQualityForItem(Item item) throws Exception {
        switch(item.name.substring(0,9)) {
            case "Aged Brie":
                updateNonDegradingItemQuality(item);
                break;
            case "Sulfuras,":
                updateLegendaryItemQuality(item);
                break;
            case "Backstage":
                updateBackstagePassQuality(item);
                break;
            case "Conjured ":
                updateConjuredItemQuality(item);
                break;
            default:
                updateRegularItemQuality(item);
        }
    }

    //Increase quality of items which cannot degrade
    private void updateNonDegradingItemQuality(Item item) throws Exception {
        if (item.sellIn >= 0) {
            adjustQuality(item, "increase",1);
        }
        else {
            adjustQuality(item, "increase",2);
        }
        makeSureQualityIsInRange(item, 50,0);
    }

    //Set legendary item quality to 80 as it can never be different
    private void updateLegendaryItemQuality(Item item) {
        makeSureQualityIsInRange(item, 80,80);
    }

    //Increase quality of backstage passes as the concert date nears, after the concert quality drops to 0
    private void updateBackstagePassQuality(Item item) throws Exception {
        if (item.sellIn > 10) {
            adjustQuality(item, "increase",1);
        }
        if (item.sellIn <= 10 && item.sellIn > 5) {
            adjustQuality(item, "increase",2);
        }
        if (item.sellIn <= 5 && item.sellIn >= 0) {
            adjustQuality(item, "increase",3);
        }
        if (item.sellIn < 0) {
            adjustQuality(item, "set",0);
        }
        makeSureQualityIsInRange(item, 50,0);
    }

    //Decrease quality of conjured items fast
    private void updateConjuredItemQuality(Item item) throws Exception {
        if (item.sellIn >= 0) {
            adjustQuality(item, "decrease",2);
        }
        else {
            adjustQuality(item, "decrease",4);
        }
        makeSureQualityIsInRange(item, 50,0);
    }

    //Decrease quality of item types not specified above
    private void updateRegularItemQuality(Item item) throws Exception {
        if (item.sellIn >= 0) {
            adjustQuality(item, "decrease",1);
        }
        else {
            adjustQuality(item, "decrease",2);
        }
        makeSureQualityIsInRange(item, 50,0);
    }

    //Adjust the item quality with previously specified value
    private void adjustQuality(Item item, String adjustment , int value) throws Exception {
        switch (adjustment) {
            case "increase":
                item.quality = item.quality + value;
                break;
            case "decrease":
                item.quality = item.quality - value;
                break;
            case "set":
                item.quality = value;
                break;
            default:
                throw new Exception("Unknown quality adjustment: "+adjustment);
        }
    }

    //Make sure the quality never goes beyond the set boundaries
    private void makeSureQualityIsInRange(Item item, int qualityIsNeverAbove, int qualityIsNeverBelow) {
        if (item.quality > qualityIsNeverAbove) {
            item.quality = qualityIsNeverAbove;
        }
        if (item.quality < qualityIsNeverBelow) {
            item.quality = qualityIsNeverBelow;
        }
    }
}
