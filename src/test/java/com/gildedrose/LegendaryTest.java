package com.gildedrose;

import com.gildedrose.utils.Articles;
import com.gildedrose.utils.EncapsulatedItem;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests for methods of {@link com.gildedrose.product.category.Legendary}
 * which been moved to their own test classes.
 */
class LegendaryTest extends BaseTest{

    @Test
    void tryLowerQualityAndUpQuantityByOne() {

        // given
        final int sellIn  = 2;
        final int quality = 2;
        final EncapsulatedItem item = createItem(Articles.SULFURAS.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertAll(
                () -> assertEquals(sellIn,  item.getSellIn()),
                () -> assertEquals(quality,  item.getQuality())
        );
    }

    @Test
    void increaseQualityToDouble() {

        // given
        final int sellIn  = 4;
        final int quality = 80;
        final EncapsulatedItem item = createItem(Articles.SULFURAS.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(quality,  item.getQuality());
    }

    @Test
    void tryIncreaseQuality() {

        // given
        final int sellIn  = -1;
        final int quality = 80;
        final EncapsulatedItem item = createItem(Articles.SULFURAS.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(quality,  item.getQuality());
    }
}
