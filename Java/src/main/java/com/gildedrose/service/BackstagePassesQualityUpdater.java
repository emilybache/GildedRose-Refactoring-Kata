package com.gildedrose.service;

import com.gildedrose.Item;
import lombok.NoArgsConstructor;

import java.util.List;

import static com.gildedrose.enums.BorderDays.DATE_HAS_PASSED;
import static com.gildedrose.enums.BorderDays.INCREASE_BY_THREE_DAYS;
import static com.gildedrose.enums.BorderDays.INCREASE_BY_TWO_DAYS;
import static com.gildedrose.enums.BorderDays.INCREASE_QUALITY_BY_THREE_WHEN_LESS_THEN_OR_EQUAL_FIVE_DAYS;
import static com.gildedrose.enums.BorderDays.INCREASE_QUALITY_BY_TWO_WHEN_LESS_THEN_OR_EQUAL_TEN_DAYS;
import static com.gildedrose.enums.BorderQualities.MAX_QUALITY;
import static com.gildedrose.enums.BorderQualities.MIN_QUALITY;

@NoArgsConstructor
public class BackstagePassesQualityUpdater implements QualityUpdater {

    @Override
    public void updateQuality(List<Item> items) {
        for (Item item : items) {
            item.setSellIn(item.getSellIn() - 1);

            if (item.getSellIn() < DATE_HAS_PASSED.getDays()) {
                item.setQuality(0);
            }

            // quantity actually always increases as the item becomes older (if sellIn < 0 decreases twice by general condition!)
            if (item.getQuality() < MAX_QUALITY.getQuality() && item.getQuality() > MIN_QUALITY.getQuality()) {
                // Once the sell by date has passed, quality degrades twice as fast - - should this have to happen, because it is logical contradiction ???
                if (item.getSellIn() >= INCREASE_QUALITY_BY_THREE_WHEN_LESS_THEN_OR_EQUAL_FIVE_DAYS.getDays() &&
                    item.getSellIn() < INCREASE_QUALITY_BY_TWO_WHEN_LESS_THEN_OR_EQUAL_TEN_DAYS.getDays() &&
                    item.getQuality() + INCREASE_BY_TWO_DAYS.getDays() <= MAX_QUALITY.getQuality()) {
                    item.setQuality(item.getQuality() + INCREASE_BY_TWO_DAYS.getDays());
                } else if (item.getSellIn() < INCREASE_QUALITY_BY_THREE_WHEN_LESS_THEN_OR_EQUAL_FIVE_DAYS.getDays() &&
                    item.getQuality() + INCREASE_BY_THREE_DAYS.getDays() <= MAX_QUALITY.getQuality()) {
                    item.setQuality(item.getQuality() + INCREASE_BY_THREE_DAYS.getDays());
                } else {
                    item.setQuality(item.getQuality() + 1);
                }
            }
        }
    }
}
