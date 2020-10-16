import unittest
import ../src/items
import ../src/gildedrose

let list = @[
  initItem("+5 Dexterity Vest", 10, 20),
  initItem("Aged Brie", 2, 0),
  # this doesn't have any special treatment, but if you want you can think of something and add a test
  initItem("Elixir of the Mongoose", 5, 7),
  initItem("Sulfuras, Hand of Ragnaros", 0, 80),
  initItem("Sulfuras, Hand of Ragnaros", -1, 80),
  initItem("Backstage passes to a TAFKAL80ETC concert", 15, 20),
  initItem("Backstage passes to a TAFKAL80ETC concert", 10, 49),
  initItem("Backstage passes to a TAFKAL80ETC concert", 5, 49),
  # this conjured item does not work properly yet
  initItem("Conjured Mana Cake", 3, 6)
]

template checkItem(item: Item, s, q: int) =
  check:
    item.sellIn == s
    item.quality == q

proc days(count: int): seq[Item] =
    var items = list
    for _ in 1 .. count:
      items.updateQuality()
    items

suite "+5 Dexterity Vest [0]":

  test "1 day":
    var items = days 1
    checkItem items[0], 9, 19

  test "5 days, check for cumulative depeciation":
    let items = days 5
    checkItem items[0], 5, 15
  
  test "10 days, expiration day":
    let items = days 10
    checkItem items[0], 0, 10

  test "14 days, check for expired depreciation":
    let items = days 14
    checkItem items[0], -4, 2

  test "20 days, quality should not be negative":
    let items = days 20
    checkItem items[0], -10, 0

suite "Aged Brie [1]":
  test "1 day, brie increases value":
    let items = days 1
    checkItem items[1], 1, 1

  test "2 days, expiration day":
    let items = days 2
    checkItem items[1], 0, 2

  test "4 days, extra increase after expiration":
    let items = days 4
    checkItem items[1], -2, 6

  test "30 days, should reach max quality":
    let items = days 30
    checkItem items[1], -28, 50

suite "Sulfuras, Hand of Ragnaros [3]":
  test "1 day":
    let items = days 1
    checkItem items[3], 0, 80

  test "2 days":
    let items = days 2
    checkItem items[3], 0, 80

suite "Sulfuras, Hand of Ragnaros [4]":
  test "1 day":
    let items = days 1
    checkItem items[4], -1, 80


suite "Backstage passes to a TAFKAL80ETC concert [5]":
  test "1 day":
    let items= days 1
    checkItem items[5], 14, 21
  
  test "5 days":
    let items = days 5
    checkItem items[5], 10, 25

  test "7 days, increase appreciation":
    let items = days 7
    checkItem items[5], 8, 29

  test "12 days, extra increased appriciation":
    let items = days 12
    checkItem items[5], 3, 41
  
  test "14 days, day before concert":
    let items = days 14
    checkItem items[5], 1, 47

  test "16 days, day after concert":
    let items = days 16
    checkItem items[5], -1, 0

suite "Backstage passes to a TAFKAL80ETC concert [6]":
  test "1 day, should reach max quality even though concert is soon":
    let items = days 1
    checkItem items[6], 9, 50

  test "11 days, day after concert":
    let items = days 11
    checkItem items[6], -1, 0

suite "Backstage passes to a TAFKAL80ETC concert [7]":
  test "1 day, should reach max quality even though concert is imminent":
    let items = days 1
    checkItem items[7], 4, 50

suite "Conjured Mana cake [8]":
  test "1 day, should decrease by double":
    let items = days 1
    checkItem items[8], 2, 4
