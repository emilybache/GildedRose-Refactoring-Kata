import gilded_rose_item.{Item, update_quality}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn update_quality_test() {
  let inventory = [Item("foo", 0, 0)]
  let assert [new_item] = update_quality(inventory)
  new_item.name |> should.equal("fixme")
}
