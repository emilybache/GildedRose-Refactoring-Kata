defmodule GildedRoseTest do
  use ExUnit.Case
  import GildedRose

  test "Items Quality decreases by 1" do
    assert_update_item(
      %Item{name: "Item name", sell_in: 9, quality: 3},
      %Item{name: "Item name", sell_in: 8, quality: 2}
    )
    assert_update_item(
      %Item{name: "Item name", sell_in: 1, quality: 7},
      %Item{name: "Item name", sell_in: 0, quality: 6}
    )
  end

  test "Once the sell by date has passed, Quality degrades twice as fast" do
    assert_update_item(
      %Item{name: "Item name", sell_in: 0, quality: 5},
      %Item{name: "Item name", sell_in: -1, quality: 3}
    )
    assert_update_item(
      %Item{name: "Item name", sell_in: -1, quality: 8},
      %Item{name: "Item name", sell_in: -2, quality: 6}
    )
  end

  test "The Quality of an item is never negative" do
    assert_update_item(
      %Item{name: "Item name", sell_in: 4, quality: 0},
      %Item{name: "Item name", sell_in: 3, quality: 0}
    )
    assert_update_item(
      %Item{name: "Item name", sell_in: 0, quality: 0},
      %Item{name: "Item name", sell_in: -1, quality: 0}
    )
    assert_update_item(
      %Item{name: "Item name", sell_in: -2, quality: 0},
      %Item{name: "Item name", sell_in: -3, quality: 0}
    )
  end

	test "Aged Brie actually increases in Quality the older it gets" do
    assert_update_item(
      %Item{name: "Aged Brie", sell_in: 12, quality: 8},
      %Item{name: "Aged Brie", sell_in: 11, quality: 9}
    )
    assert_update_item(
      %Item{name: "Aged Brie", sell_in: 4, quality: 6},
      %Item{name: "Aged Brie", sell_in: 3, quality: 7}
    )
    assert_update_item(
      %Item{name: "Aged Brie", sell_in: 1, quality: 10},
      %Item{name: "Aged Brie", sell_in: 0, quality: 11}
    )
  end

  test "[DISCOVERED] Quality of Aged Brie increases by 2 once the sell by date has passed" do
    assert_update_item(
      %Item{name: "Aged Brie", sell_in: 0, quality: 7},
      %Item{name: "Aged Brie", sell_in: -1, quality: 9}
    )
    assert_update_item(
      %Item{name: "Aged Brie", sell_in: -2, quality: 9},
      %Item{name: "Aged Brie", sell_in: -3, quality: 11}
    )
  end

	test "The Quality of an item is never more than 50" do
    assert_update_item(
      %Item{name: "Aged Brie", sell_in: 7, quality: 50},
      %Item{name: "Aged Brie", sell_in: 6, quality: 50}
    )
    assert_update_item(
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 9, quality: 50},
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 8, quality: 50}
    )
  end

  test "Quality of backstage passes increases by 2 when there are 10 days or less" do
    assert_update_item(
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 9, quality: 1},
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 8, quality: 3}
    )
  end

  test "Quality of backstag drops to 0 after the concert" do
    assert_update_item(
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 0, quality: 12},
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: -1, quality: 0}
    )
    assert_update_item(
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: -2, quality: 10},
      %Item{name: "Backstage passes to a TAFKAL80ETC concert", sell_in: -3, quality: 0}
    )
  end

  defp assert_update_item(item, expected) do
    assert update_item(item) == expected
  end

end
