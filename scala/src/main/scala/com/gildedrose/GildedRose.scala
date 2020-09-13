package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def updateQuality() {
    for (i <- items.indices) {
      if (isItemNotIncremental(i)) {
        incrementQualityForNonIncrementalItems(i)
      } else
        incrementQualityForIncrementalItems(i)
      decrementSellInIfNotSulfuras(i)
      if (isSellInLessThanThreshold(i, 0))
        updateQualityIfSellInZero(i)
    }
  }

  private def incrementQualityForNonIncrementalItems(i: Int): Unit = {
    if (isQualityPositive(i)) {
      incrementQuality(i, -1)
      additionalIncrementForConjured(i)
    }
  }

  private def incrementQualityForIncrementalItems(i: Int): Unit = {
    if (isQualityLessThan50(i)) {
      incrementQuality(i, 1)
      additionalIncrementForBackstagePasses(i)
    }
  }

  private def updateQualityIfSellInZero(i: Int): Unit = {
    if (isAgedBrie(i))
      incrementQualityBy1IfLessThan50(i)
    else if (isBackstagePass(i))
      dropQualityTo0(i)
    else if (!isSulfuras(i) && isQualityPositive(i))
      incrementQuality(i, -1)
  }

  private def additionalIncrementForConjured(i: Int): Unit = {
    if (isConjured(i))
      incrementQuality(i, -1)
  }

  private def additionalIncrementForBackstagePasses(i: Int): Unit = {
    if (isBackstagePass(i))
      incrementBackstagePassQuality(i)
  }

  private def incrementBackstagePassQuality(i: Int): Unit = {
    incrementForBackstagePassesIfSellInBetween5and10(i)
    incrementForBackstagePassesIfSellInBetween0and5(i)
  }

  private def incrementForBackstagePassesIfSellInBetween0and5(i: Int): Unit = {
    if (isSellInLessThanThreshold(i, 6))
      incrementQualityBy1IfLessThan50(i)
  }

  private def incrementForBackstagePassesIfSellInBetween5and10(i: Int): Unit = {
    if (isSellInLessThanThreshold(i, 11))
      incrementQualityBy1IfLessThan50(i)
  }

  private def decrementSellInIfNotSulfuras(i: Int): Unit = {
    if (!isSulfuras(i))
      decrementSellIn(i)
  }

  private def incrementQualityBy1IfLessThan50(i: Int): Unit = {
    if (isQualityLessThan50(i))
      incrementQuality(i, 1)
  }

  private def isSellInLessThanThreshold(i: Int, threshold: Int) = {
    items(i).sellIn < threshold
  }

  private def decrementSellIn(i: Int): Unit = {
    items(i).sellIn = items(i).sellIn - 1
  }

  private def isQualityPositive(i: Int) = {
    items(i).quality > 0
  }

  private def isQualityLessThan50(i: Int) = {
    items(i).quality < 50
  }

  private def incrementQuality(i: Int, delta: Int): Unit = {
    items(i).quality = items(i).quality + delta
  }

  private def dropQualityTo0(i: Int): Unit = {
    items(i).quality = 0
  }

  private def isItemNotIncremental(i: Int) = {
    !isAgedBrie(i) && !isBackstagePass(i) && !isSulfuras(i)
  }

  private def isBackstagePass(i: Int) = {
    items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")
  }

  private def isAgedBrie(i: Int) = {
    items(i).name.equals("Aged Brie")
  }

  private def isSulfuras(i: Int) = {
    items(i).name.equals("Sulfuras, Hand of Ragnaros")
  }

  private def isConjured(i: Int) = {
    items(i).name.equals("Conjured Mana Cake")
  }
}