defmodule GildedRose.TextTestFixture do

  def run(days) do
    IO.puts("OMGHAI!")

    items = [
      %Item{name: "+5 Dexterity Vest", sell_in: 10, quality: 20},
      %Item{name: "Aged Brie", sell_in: 2, quality: 0},
      %Item{name: "Elixir of the Mongoose", sell_in: 5, quality: 7},
      %Item{name: "Sulfuras, Hand of Ragnaros", sell_in: 0, quality: 80},
      %Item{name: "Sulfuras, Hand of Ragnaros", sell_in: -1, quality: 80},
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 15, quality: 20},
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 10, quality: 49},
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 5, quality: 49},
      # This Conjured item does not work properly yet
      %Item{name: "Conjured Mana Cake", sell_in: 3, quality: 6}, # <-- :O
    ]

    Enum.reduce(0..days, items, fn day, items ->
      IO.puts "-------- day #{day} --------"
      IO.puts "name, sellIn, quality"
      Enum.each(items, fn item -> IO.puts("#{item.name}, #{item.sell_in}, #{item.quality}") end)
      IO.puts("")
      GildedRose.update_quality(items)
    end)
  end

  def run() do
    run(2)
  end

end
