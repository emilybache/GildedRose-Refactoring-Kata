import argv
import gilded_rose_item.{type GildedRose}
import gleam/function
import gleam/io
import glint
import glint/flag
import test_fixture.{simulate}

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
