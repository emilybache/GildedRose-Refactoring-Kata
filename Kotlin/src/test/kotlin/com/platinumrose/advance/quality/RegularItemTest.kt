package com.platinumrose.advance.quality

import com.platinumrose.ItemConstant
import com.platinumrose.ItemType
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import kotlin.random.Random

class RegularItemTest {

    private val Random = Random(Int.MAX_VALUE)
    private val item = RegularItem()

    @Test
    fun `should return expected type`() {
        assertEquals(item.type(), ItemType.REGULAR)
    }

    @Test
    fun `should return expected max quality`() {
        assertEquals(item.maxQuality(), ItemConstant.REGULAR_ITEM_MAX_QUALITY)
    }

    @Test
    fun `should return -1 when calculate sell in decrease`() {
        assertEquals(item.computeSellInDecrease(Random.nextInt()), -1)
    }

    @Test
    fun `should return -1 when calculate quality increase before sell in`() {
        assertEquals(item.computeQualityIncreaseBeforeSellIn(Random.nextInt(), Random.nextInt()), -1)
    }

    @Test
    fun `should return -1 when calculate quality increase after sell in`() {
        assertEquals(item.computeQualityIncreaseAfterSellIn(Random.nextInt(), Random.nextInt()), -1)
    }
}