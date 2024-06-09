import argv
import gilded_rose.{type GildedRose, type Item, Item, update_quality}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import glint
import glint/flag

pub fn main() {
  run_cli_app(update_quality)
}

pub fn run_cli_app(modify_inventory: fn(GildedRose) -> GildedRose) {
  let days_flag = "days"

  let number_of_days =
    flag.int()
    |> flag.default(2)
    |> flag.description("Number of days")

  let simulate_inventory = fn() {
    use input <- glint.command()

    let assert Ok(number_of_days) =
      flag.get_int(from: input.flags, for: days_flag)
    simulate(number_of_days, modify_inventory)
  }

  let app =
    glint.new()
    |> glint.with_name("Gilded Rose")
    |> glint.group_flag([], days_flag, number_of_days)
    |> glint.add(at: [], do: simulate_inventory())

  io.println("OMGHAI!")
  glint.run_and_handle(app, argv.load().arguments, function.identity)
}

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
