package com.gildedrose;

import org.approvaltests.combinations.CombinationApprovals;
import org.junit.jupiter.api.Test;

import java.util.stream.IntStream;

class GoldenMasterApprovalTest {

    @Test
    void
    gildedRoseApprovalTest() {
        CombinationApprovals.verifyAllCombinations(
            this::updateItem,
            itemTypes(),
            qualityRangeValues(),
            sellInRangeValues()
        );
    }

    private String[] itemTypes() {
        return new String[]{"common item", "Aged Brie", "Backstage passes to a TAFKAL80ETC concert", "Sulfuras, Hand of Ragnaros"};
    }

    private Integer[] sellInRangeValues() {
        return IntStream.range(-1, 12).boxed().toArray(Integer[]::new);
    }

    private Integer[] qualityRangeValues() {
        return IntStream.range(0, 51).boxed().toArray(Integer[]::new);
    }

    private Item updateItem(String name, int quality, int sellIn) {
        final Item item = new ItemBuilder()
            .setName(name)
            .setSellIn(sellIn)
            .setQuality(quality)
            .createItem();
        new GildedRose(new Item[]{item}).updateQuality();
        return item;
    }
}
