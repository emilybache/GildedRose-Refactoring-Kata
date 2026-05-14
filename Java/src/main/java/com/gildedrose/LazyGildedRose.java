package java.com.gildedrose;

import com.gildedrose.Item;

/*  If the amout of items is substantially bigger than day-to-day item removing/adding,
    we can calculate item quality on the fly,
    instead of constantly decrementing every item quality, by using timestamps:

 */
public class LazyGildedRose extends com.gildedrose.GildedRose {

    Long startUnixTime;

    public LazyGildedRose(Item[] items) {
        super(items);
        this.startUnixTime = System.currentTimeMillis()/1000;
    }

    //Adjust item quality as if item was placed in the store from day one:
    private int getDaysSinceStart() {
        var timePassed = System.currentTimeMillis() / 1000 - startUnixTime;
        return Math.toIntExact(timePassed) / 24 / 3600;
    }

    private void adjustItemQuality(Item item, int daysPassed) {
        switch(item.name) {
            case AGED_BRIE, CONCERT: item.quality += daysPassed;
                break;
            case SULFURAS:;
                break;
            case CONJURED: item.quality -= daysPassed * 2;
                break;
            default: item.quality -= daysPassed;
        }
    }

    public void addItem(Item item) {
        var daysPassedSinceStart = getDaysSinceStart();
        adjustItemQuality(item, -daysPassedSinceStart);
        item.sellIn += daysPassedSinceStart;
    }

    public void sellItem(Item item) {
        var daysPassedSinceStart = getDaysSinceStart();
        adjustItemQuality(item, daysPassedSinceStart);

        var daysLeft = daysPassedSinceStart - item.sellIn;
        if(item.name.equals(CONCERT)) {
            if(daysLeft < 0) {
                item.quality = 0;
            } else {
                adjustItemQuality(item, Math.max(CONCERT_HOT_DAYS - daysLeft, 0) +  Math.max(CONCERT_HOTTEST_DAYS - daysLeft, 0));
            }
        }
        //For degrading and outdated items, adjust quality one more time:
        else if(isDegrading(item) && daysPassedSinceStart > item.sellIn) {
            adjustItemQuality(item, daysLeft);
        }
        normalizeQuality(item);
    }

    private void normalizeQuality(Item item) {
        if (item.quality < MIN_QUALITY) {
            item.quality = MIN_QUALITY;
        }
        if (item.quality > MAX_QUALITY) {
            item.quality = MAX_QUALITY;
        }
    }
}
