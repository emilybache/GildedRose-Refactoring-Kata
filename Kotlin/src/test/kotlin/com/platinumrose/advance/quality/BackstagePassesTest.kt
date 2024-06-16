package com.platinumrose.advance.quality

import com.platinumrose.ItemConstant.Companion.REGULAR_ITEM_MAX_QUALITY
import com.platinumrose.ItemType.BACKSTAGE_PASSES
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import kotlin.random.Random

class BackstagePassesTest {

    private val Random = Random(0)
    private val item = BackstagePasses()

    @Test
    fun `should return expected type`() {
        assertEquals(item.type(), BACKSTAGE_PASSES)
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
    fun `should return 1 when calculate quality increase before sell in and quality greater than 10`() {
        assertEquals(item.computeQualityIncreaseBeforeSellIn(Random.nextInt(12, Int.MAX_VALUE), Random.nextInt()), 1)
    }

    @Test
    fun `should return 2 when calculate quality increase before sell in and quality between 6 and 10`() {
        assertEquals(item.computeQualityIncreaseBeforeSellIn(6, Random.nextInt()), 2)
        assertEquals(item.computeQualityIncreaseBeforeSellIn(10, Random.nextInt()), 2)
    }

    @Test
    fun `should return 3 when calculate quality increase before sell in and quality between 0 and 5`() {
        assertEquals(item.computeQualityIncreaseBeforeSellIn(0, Random.nextInt()), 3)
        assertEquals(item.computeQualityIncreaseBeforeSellIn(5, Random.nextInt()), 3)
    }

    @Test
    fun `should return negative quality when calculate quality increase after sell in`() {
        val quality = Random.nextInt()
        assertEquals(item.computeQualityIncreaseAfterSellIn(Random.nextInt(), quality), -quality)
    }
}