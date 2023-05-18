package com.gildedrose

object TexttestFixture {
  def main(args: Array[String]): Unit = {
    val items = Array[Item](
      new Item("Sports Memorabilia", 10, 20),
      new Item("Aged Cheese", 2, 0),
      new Item("Coffee Table Book", 5, 7),
      new Item("Fine Italian Silk", 0, 80),
      new Item("Fine Italian Silk", -1, 80),
      new Item("Backstage passes to a concert", 15, 20),
      new Item("Backstage passes to a concert", 10, 49),
      new Item("Backstage passes to a concert", 5, 49),
      // this Baked item does not work properly yet
      new Item("Baked Chocolate Cake", 3, 6)
    )
    val app = new GildedRose(items)
    val days = if (args.length > 0) args(0).toInt + 1 else 2
    for (i <- 0 until days) {
      System.out.println("-------- day " + i + " --------")
      System.out.println("name, sellIn, quality")
      for (item <- items) {
        System.out.println(item.name + ", " + item.sellIn + ", " + item.quality)
      }
      System.out.println()
      app.updateQuality()
    }
  }
}
