package com.gildedrose

class ItemCreator {

  private def item(name: String, sellIn: Int, quality: Int): Item = {
    new Item(name, sellIn, quality)
  }

  def agedBrieItem(sellIn: Int, quality: Int): Item = {
    item("Aged Brie", sellIn, quality)
  }

  def sulfurasItem(): Item = {
    item("Sulfuras, Hand of Ragnaros", 1, 80)
  }

  def backstagePassItem(sellIn: Int, quality: Int): Item = {
    item("Backstage passes to a TAFKAL80ETC concert", sellIn, quality)
  }

  def elixirItem(sellIn: Int, quality: Int): Item = {
    item("Elixir of the Mongoose", sellIn, quality)
  }

  def dexterityItem(sellIn: Int, quality: Int): Item = {
    item("+5 Dexterity Vest", sellIn, quality)
  }

  def conjuredItem(sellIn: Int, quality: Int): Item = {
    item("Conjured Mana Cake", sellIn, quality)
  }

}
