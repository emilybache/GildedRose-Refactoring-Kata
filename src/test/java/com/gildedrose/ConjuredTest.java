package com.gildedrose;

import com.gildedrose.utils.Articles;
import com.gildedrose.utils.EncapsulatedItem;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Unit tests for methods of {@link com.gildedrose.product.category.Conjured}
 * which been moved to their own test classes.
 */
class ConjuredTest extends BaseTest{

    @Test
    void lowerQualityByTwoAndUpQuantityByOne() {

        // given
        final int sellIn  = 4;
        final int quality = 4;
        final EncapsulatedItem item = createItem(Articles.CONJURED.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertAll(
                () -> assertEquals(sellIn -1,  item.getSellIn()),
                () -> assertEquals(quality - 2,  item.getQuality())
        );
    }

    @Test
    void increaseQualityToDouble() {

        // given
        final int sellIn  = 0;
        final int quality = 2;
        final EncapsulatedItem item = createItem(Articles.AGED_BRIE.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(quality + 2,  item.getQuality());
    }

    @Test
    void tryUpdateQualityAboveMax() {

        // given
        final int sellIn  = 4;
        final int quality = 50;
        final EncapsulatedItem item = createItem(Articles.AGED_BRIE.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertEquals(50,  item.getQuality(), "The Quality of an item is never more than 50");
    }


}
