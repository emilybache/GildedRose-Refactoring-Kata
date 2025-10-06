package com.gildedrose

object TexttestFixture {
  def main(args: Array[String]): Unit = {
    System.out.println("OMGHAI!")

    val items = Array[Item](
        Item("+5 Dexterity Vest", 10, 20),
        Item("Aged Brie", 2, 0),
        Item("Elixir of the Mongoose", 5, 7),
        Item("Sulfuras, Hand of Ragnaros", 0, 80),
        Item("Sulfuras, Hand of Ragnaros", -1, 80),
        Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
        Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
        Item("Backstage passes to a TAFKAL80ETC concert", 5, 49), // this conjured item does not work properly yet
        Item("Conjured Mana Cake", 3, 6)
    )

    val app  = new GildedRose(items)
    var days = 2

    if (args.nonEmpty) days = args(0).toInt + 1

    for (i <- 0 until days) {
      System.out.println("-------- day " + i + " --------")
      System.out.println("name, sellIn, quality")
      for (item <- items) {
        System.out.println(s"${item.name}, ${item.sellIn}, ${item.quality}")
      }
      System.out.println()
      app.updateQuality()
    }
  }
}
