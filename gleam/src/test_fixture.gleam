import gilded_rose_item.{type GildedRose, type Item, Item}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

const test_fixture = [
  Item("+5 Dexterity Vest", 10, 20), Item("Aged Brie", 2, 0),
  Item("Elixir of the Mongoose", 5, 7),
  Item("Sulfuras, Hand of Ragnaros", 0, 80),
  Item("Sulfuras, Hand of Ragnaros", -1, 80),
  Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
  Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
  Item("Backstage passes to a TAFKAL80ETC concert", 5, 49),
  Item("Conjured Mana Cake", 3, 6),
]

pub fn simulate(
  number_of_days: Int,
  modify_inventory: fn(GildedRose) -> GildedRose,
) -> GildedRose {
  let days = list.range(0, number_of_days)
  let display_item = fn(item: Item) {
    string.join(
      [item.name, int.to_string(item.sell_in), int.to_string(item.quality)],
      with: ", ",
    )
  }
  list.fold(days, from: test_fixture, with: fn(inventory, day) {
    case day != 0 {
      True -> io.println("")
      False -> Nil
    }
    io.println(string.join(
      ["-------- day", int.to_string(day), "--------"],
      with: " ",
    ))
    io.println("name, sellIn, quality")
    list.each(inventory, fn(item) { io.println(display_item(item)) })
    modify_inventory(inventory)
  })
}
