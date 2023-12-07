import unittest
import ../src/items
import ../src/gildedrose

suite "Gilded Rose":

  test "foo":
    var items = @[initItem("foo", 0, 0)]
    items.updateQuality()
    check items[0].name == "fixme"
