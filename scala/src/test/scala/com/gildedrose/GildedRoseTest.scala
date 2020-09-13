package com.gildedrose

import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GildedRoseTest extends AnyWordSpec with Matchers with BeforeAndAfter {

  private var itemCreator: ItemCreator = _

  private var app: GildedRose = _
  before {
    itemCreator = new ItemCreator()
  }

  "quality of item is never negative" in {
    val sellIn = 10
    val quality = 1
    app = createApp(createItemsOfAllTypes(sellIn, quality))
    verifyQualityIsPositive(sellIn + 2)
  }

  private def createItemsOfAllTypes(sellIn: Int, quality: Int) = {
    Array[Item](
      itemCreator.backstagePassItem(sellIn, quality),
      itemCreator.agedBrieItem(sellIn, quality),
      itemCreator.dexterityItem(sellIn, quality),
      itemCreator.elixirItem(sellIn, quality),
      itemCreator.sulfurasItem()
    )
  }

  private def verifyQualityIsPositive(sellIn: Int): Unit = {
    for (i <- 1 to sellIn) {
      app.updateQuality()
      for (j <- app.items)
        assert(j.quality >= 0)
    }
  }

  "quality is never more than 50 except for Sulfuras" in {
    val sellIn = 10
    val quality = 48
    app = createApp(createItemsWithoutSulfuras(sellIn, quality))
    verifyQualityLessThanOrEqualTo50(sellIn + 2)
  }

  private def createItemsWithoutSulfuras(sellIn: Int, quality: Int) = {
    Array[Item](
      itemCreator.backstagePassItem(sellIn, quality),
      itemCreator.agedBrieItem(sellIn, quality),
      itemCreator.dexterityItem(sellIn, quality),
      itemCreator.elixirItem(sellIn, quality)
    )
  }

  private def verifyQualityLessThanOrEqualTo50(sellIn: Int): Unit = {
    for (i <- 1 to sellIn) {
      app.updateQuality()
      for (j <- app.items)
        assert(j.quality <= 50)
    }
  }

  "quality decreases twice as fast after sell date has passed" in {
    val sellIn = 1
    val quality = 10
    app = createApp(createItemsOfDecreasingQuality(sellIn, quality))
    verifyQualityDroppedTwiceAsFast(quality)
  }

  private def createItemsOfDecreasingQuality(sellIn: Int, quality: Int) = {
    Array[Item](
      itemCreator.dexterityItem(sellIn, quality),
      itemCreator.elixirItem(sellIn, quality)
    )
  }

  private def verifyQualityDroppedTwiceAsFast(quality: Int): Unit = {
    verifyQualityDecreasedByDiff(quality, 1)
    verifyQualityDecreasedByDiff(quality, 3)
    verifyQualityDecreasedByDiff(quality, 5)
  }

  private def verifyQualityDecreasedByDiff(quality: Int, diff: Int): Unit = {
    app.updateQuality()
    for (i <- app.items)
      assert(i.quality == quality - diff)
  }

  "quality of Conjured items degrade twice as fast of elixir" in {
    val sellIn = 1
    val quality = 30
    app = createApp(createItemsWithConjuredAndElixir(sellIn, quality))
    verifyQualityForConjuredAgainstElixir(sellIn)
  }

  private def createItemsWithConjuredAndElixir(sellIn: Int, quality: Int) = {
    Array[Item](
      itemCreator.conjuredItem(sellIn, quality),
      itemCreator.elixirItem(sellIn, quality)
    )
  }

  private def verifyQualityForConjuredAgainstElixir(sellIn: Int): Unit = {
    for (i <- 1 to sellIn) {
      val prevQualityConjured = app.items(0).quality
      val prevQualityElixir = app.items(1).quality
      app.updateQuality()
      val conjuredQualityDrop = prevQualityConjured - app.items(0).quality
      val elixirQualityDrop = prevQualityElixir - app.items(1).quality
      assert(conjuredQualityDrop == 2 * elixirQualityDrop)
    }
  }

  "quality of Aged Brie increases with time and stays below 50" in {
    app = createApp(createItemsWithOnlyAgedBrie())
    verifyQualityForAgedBrie()
  }

  private def createItemsWithOnlyAgedBrie() = {
    Array[Item](itemCreator.agedBrieItem(5, 48))
  }

  private def verifyQualityForAgedBrie(): Unit = {
    var quality = app.items(0).quality
    for (i <- 1 to 40) {
      app.updateQuality()
      assert(app.items(0).quality <= 50)
      if (quality < 50) {
        assert(app.items(0).quality > quality)
        quality = app.items(0).quality
      }
    }
  }

  "sulfuras never has to be sold and quality stays at 80" in {
    app = createApp(createItemsWithOnlySulfuras())
    val sellIn = app.items(0).sellIn
    verifyQualityForSulfuras(sellIn)
  }

  private def createItemsWithOnlySulfuras() = {
    Array[Item](itemCreator.sulfurasItem())
  }

  private def verifyQualityForSulfuras(sellIn: Int): Unit = {
    for (i <- 1 to sellIn) {
      app.updateQuality()
      verifyIfQualityAndSellInFixed(sellIn)
    }
  }

  private def verifyIfQualityAndSellInFixed(sellIn: Int): Unit = {
    for (j <- app.items) {
      assert(j.quality == 80)
      assert(j.sellIn == sellIn)
    }
  }

  "quality for backstage passes" in {
    val quality = 20
    app = createApp(createItemsWithOnlyBackstagePass(quality))
    verifyQualityForBackstagePasses(quality)
  }

  private def createItemsWithOnlyBackstagePass(quality: Int) = {
    Array[Item](itemCreator.backstagePassItem(10, quality))
  }

  private def verifyQualityForBackstagePasses(quality: Int) = {
    verifyIfQualityDecreasedBy2WhenSellInBetween5and10(quality)
    verifyIfQualityDecreasedBy3WhenSellInLessThan5(quality)
    verifyIfQualityDroppedTo0()
  }

  private def verifyIfQualityDecreasedBy2WhenSellInBetween5and10(quality: Int): Unit = {
    for (i <- 1 to 5) {
      app.updateQuality()
      assert(app.items(0).quality == quality + 2 * i)
    }
  }

  private def verifyIfQualityDecreasedBy3WhenSellInLessThan5(quality: Int): Unit = {
    for (i <- 1 to 5) {
      app.updateQuality()
      assert(app.items(0).quality == quality + 10 + 3 * i)
    }
  }

  private def verifyIfQualityDroppedTo0() = {
    app.updateQuality()
    assert(app.items(0).quality == 0)
  }

  private def createApp(items: Array[Item]) = {
    app = new GildedRose(items)
    app
  }
}