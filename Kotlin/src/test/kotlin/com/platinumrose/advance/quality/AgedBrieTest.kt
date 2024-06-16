package com.platinumrose.advance.quality

import com.platinumrose.ItemConstant.Companion.REGULAR_ITEM_MAX_QUALITY
import com.platinumrose.ItemType
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import kotlin.random.Random

class AgedBrieTest {

    private val Random = Random(Int.MAX_VALUE)
    private val item = AgedBrie()

    @Test
    fun `should return expected type`() {
        assertEquals(item.type(), ItemType.AGED_BRIE)
    }

    @Test
    fun `should return expected max quality`() {
        assertEquals(item.maxQuality(), REGULAR_ITEM_MAX_QUALITY)
    }

    @Test
    fun `should return -1 when calculate sell in decrease`() {
        assertEquals(item.computeSellInDecrease(Random.nextInt()), -1)
    }

    @Test
    fun `should return 1 when calculate quality increase before sell in`() {
        assertEquals(item.computeQualityIncreaseBeforeSellIn(Random.nextInt(), Random.nextInt()), 1)
    }

    @Test
    fun `should return 1 when calculate quality increase after sell in`() {
        assertEquals(item.computeQualityIncreaseAfterSellIn(Random.nextInt(), Random.nextInt()), 1)
    }
}