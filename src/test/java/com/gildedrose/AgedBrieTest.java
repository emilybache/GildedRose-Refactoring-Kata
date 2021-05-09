package com.gildedrose;

import com.gildedrose.utils.Articles;
import com.gildedrose.utils.EncapsulatedItem;
import org.junit.jupiter.api.Test;


import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Unit tests for methods of {@link com.gildedrose.product.category.AgedBrie}
 * which been moved to their own test classes.
 */
class AgedBrieTest extends BaseTest{

    @Test
    void lowerQualityAndUpQuantityByOne() {

        // given
        final int sellIn  = 2;
        final int quality = 2;
        final EncapsulatedItem item = createItem(Articles.AGED_BRIE.getArticle(), sellIn, quality);

        // when
        updateQuality(item);

        // then
        assertAll(
                () -> assertEquals(sellIn -1,  item.getSellIn()),
                () -> assertEquals(quality + 1,  item.getQuality())
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
        final int sellIn  = 2;
        final int quality = 50;
        final EncapsulatedItem item = createItem(Articles.AGED_BRIE.getArticle(), sellIn, quality);
        // when
        updateQuality(item);

        // then
        assertEquals(50,  item.getQuality(), "The Quality of an item is never more than 50");
    }

    @Test
    void toStringEncapsulateItem() {
        // given
        final int sellIn  = 2;
        final int quality = 2;
        final EncapsulatedItem item = createItem(Articles.AGED_BRIE.getArticle(), sellIn, quality);
        // when
        updateQuality(item);
        // then
        assertEquals("Aged Brie, " + (sellIn - 1) + ", " + (quality + 1),  item.toString());
    }

    @Test
    void checkNullItems() {
        // given
        final int sellIn  = 2;
        final int quality = 2;
        final EncapsulatedItem item = createItem(Articles.AGED_BRIE.getArticle(), sellIn, quality);
        // when
        // then
        assertThrows(NullPointerException.class, () -> updateQuality(item, null));
    }

    @Test
    void checkNullItemsInsideArray() {
        // given
        // when
        // then
        assertThrows(NullPointerException.class, () -> updateQuality((EncapsulatedItem) null));
    }

}
