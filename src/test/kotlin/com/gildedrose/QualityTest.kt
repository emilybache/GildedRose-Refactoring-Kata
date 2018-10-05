package com.gildedrose

import com.gildedrose.core.TestUtils
import com.gildedrose.core.TestUtils.generateRandomName
import com.gildedrose.core.TestUtils.generateRandomNumber
import com.gildedrose.core.TestUtils.pickRandomItem
import com.gildedrose.core.advanceTimeBy
import org.assertj.core.api.Java6Assertions.assertThat
import org.junit.Before
import org.junit.Test

class QualityTest {

    private lateinit var store: Array<Item>

    @Before
    fun setUp() {
        store = TestUtils.fixture
    }

    @Test
    fun name() {
        throw UnsupportedOperationException()
    }
}
