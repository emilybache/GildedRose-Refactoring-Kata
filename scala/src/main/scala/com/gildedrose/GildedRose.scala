package com.gildedrose

class GildedRose(val items: Array[Item]) {


  def updateQuality() {
    items.foreach(item => {
      if(item.quality > 0 && item.quality < 50) {
        item.name match {
          case "Aged Brie" => item.quality += 1

          case "Sulfuras, Hand of Ragnaros" => _

          case "Backstage passes to a TAFKAL80ETC concert" =>
            if(item.sellIn < 0) item.quality = 0
            else {
              item.quality += 1
              if(item.sellIn <= 10) item.quality += 1
              if(item.sellIn <= 5) item.quality += 1
            }

          case "Conjured" =>
            item.quality -= 2
            if(item.sellIn < 0) item.quality -= 2

          case _ =>
            item.quality -= 1
            if(item.sellIn < 0) item.quality -= 1
        }
        if(item.quality > 50) item.quality = 50
        if(item.quality < 0) item.quality = 0
      }

      if(!item.name.equals("Sulfuras, Hand of Ragnaros")) item.sellIn -= 1
    })
  }
}