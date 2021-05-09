package com.gildedrose;

import com.gildedrose.utils.Articles;
import com.gildedrose.utils.EncapsulatedItem;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests for methods of {@link com.gildedrose.product.category.BackStagePass}
 * which been moved to their own test classes.
 */
public class BackStagePassTest extends BaseTest{

    @Test
    void lowerQualityAndUpQuantityByOne() {

        // given
        final int sellIn  = 15;
        final int quality = 3;
        final EncapsulatedItem item = createItem(Articles.TAFKAL_80ETC.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(quality + 1,  item.getQuality());
    }
    @Test
    void increaseQualityToDouble() {

        // given
        final int sellIn  = 9;
        final int quality = 3;
        final EncapsulatedItem item = createItem(Articles.TAFKAL_80ETC.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(quality + 2,  item.getQuality());
    }
    @Test
    void dropAfterConcert() {

        // given
        final int sellIn  = 0;
        final int quality = 3;
        final EncapsulatedItem item = createItem(Articles.TAFKAL_80ETC.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(sellIn,  item.getQuality());
    }

    @Test
    void increaseQualityToDoubleByThree() {

        // given
        final int sellIn  = 2;
        final int quality = 2;
        final EncapsulatedItem item = createItem(Articles.TAFKAL_80ETC.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(quality + 3,  item.getQuality());
    }

    @Test
    void tryUpdateQualityAboveMax() {

        // given
        final int sellIn  = 5;
        final int quality = 50;
        final EncapsulatedItem item = createItem(Articles.AGED_BRIE.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(50,  item.getQuality(), "The Quality of an item is never more than 50");
    }
}
