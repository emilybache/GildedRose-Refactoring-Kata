require File.join(File.dirname(__FILE__), 'gilded_rose')
require 'test/unit'

class TestUntitled < Test::Unit::TestCase
  MAX_QUALITY = 50

  test "Once the sell by date has passed, quality degrades twice as fast" do
    initial_expired_item_quality = 2
    initial_unexpired_item_quality = 2

    expired_item = Item.new(name = "TestItem", sell_in = 0, quality = initial_expired_item_quality)
    unexpired_item = Item.new(name = "TestItem", sell_in = 2, quality = initial_unexpired_item_quality)

    items = [expired_item, unexpired_item]
    GildedRose.new(items).update_quality

    assert_equal initial_expired_item_quality - 2, expired_item.quality
    assert_equal initial_unexpired_item_quality - 1, unexpired_item.quality
  end

  test "Once the sell by date has passed, quality degrades but it does not drop below zero" do
    expired_item = Item.new(name = "TestItem", sell_in = 0, quality = 0)

    gilded_rose = GildedRose.new([expired_item])

    gilded_rose.update_quality
    assert_equal 0, expired_item.quality
  end

  test "After every update_quality call, sell_in is reduced by 1" do
    number_of_updates = 10
    initial_item_sell_in = 4

    item = Item.new(name = "TestItem", sell_in = initial_item_sell_in, quality = 0)
    gilded_rose = GildedRose.new([item])
    number_of_updates.times { gilded_rose.update_quality }

    expected_item_sell_in = initial_item_sell_in - number_of_updates
    assert_equal expected_item_sell_in, item.sell_in
  end

  test "Aged Brie item increases in Quality the older it gets" do
    number_of_updates = 10
    initial_item_sell_in = 10
    initial_item_quality = 0

    item = Item.new(name = "Aged Brie", sell_in = initial_item_sell_in, quality = initial_item_quality)

    gilded_rose = GildedRose.new([item])
    number_of_updates.times { gilded_rose.update_quality }

    expected_item_quality = initial_item_quality + number_of_updates
    assert_equal expected_item_quality, item.quality
  end

  test "Aged Brie item increases in Quality the older it gets, but it does not exceed 50 in quality" do
    initial_item_sell_in = 10
    initial_item_quality = 0

    item = Item.new(name = "Aged Brie", sell_in = initial_item_sell_in, quality = initial_item_quality)

    gilded_rose = GildedRose.new([item])
    (MAX_QUALITY + 1).times { gilded_rose.update_quality }

    assert_equal MAX_QUALITY, item.quality
  end

  test "Sulfuras sell by date does not decrease after every update_quality" do
    number_of_updates = 20
    initial_item_sell_in = 10

    item = Item.new(name = "Sulfuras, Hand of Ragnaros", sell_in = initial_item_sell_in, quality = 10)

    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality

    assert_equal initial_item_sell_in, item.sell_in
  end

  test "Sulfuras quality does not decrease after each update_quality" do
    initial_item_quality = 10

    item = Item.new(name = "Sulfuras, Hand of Ragnaros", sell_in = 10, quality = initial_item_quality)

    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality

    assert_equal initial_item_quality, item.quality
  end

  test "Backstage passes quality increases by 2 when the item has 10 days or less to expire" do
    initial_item_quality = 10

    item = Item.new(name = "Backstage passes to a TAFKAL80ETC concert", sell_in = 10, quality = initial_item_quality)

    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality

    expected_item_quality = initial_item_quality + 2
    assert_equal expected_item_quality, item.quality
  end

  test "Backstage passes quality increases by 3 when the item has 5 days or less to expire" do
    initial_item_quality = 10

    item = Item.new(name = "Backstage passes to a TAFKAL80ETC concert", sell_in = 5, quality = initial_item_quality)

    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality

    expected_item_quality = initial_item_quality + 3
    assert_equal expected_item_quality, item.quality
  end

  test "Backstage passes quality drops to 0 when the item is expired" do
    initial_expired_item_quality = 10
    initial_expired_item_sell_in = 0
    item = Item.new(name = "Backstage passes to a TAFKAL80ETC concert", sell_in = initial_expired_item_sell_in, quality = initial_expired_item_quality)

    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality

    assert_equal 0, item.quality
  end
end
