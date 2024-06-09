import gilded_rose.{Item, update_quality}
import gleeunit/should

pub fn update_quality_test() {
  let inventory = [Item("foo", 0, 0)]
  let assert [new_item] = update_quality(inventory)
  new_item.name |> should.equal("fixme")
}
