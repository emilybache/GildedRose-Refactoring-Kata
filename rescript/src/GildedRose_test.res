open Jest
open Expect
open GildedRose

describe("Gilded Rose", () => {
  test("should foo", () => {
    let items: array<Item.t> = [{name: "foo", sellIn: 0, quality: 0}]
    let updatedItems = updateQuality(items)
    expect(updatedItems[0].name)->toBe("fixme")
  })
})
