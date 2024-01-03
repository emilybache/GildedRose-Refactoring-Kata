require_relative 'gilded_rose'
require_relative 'item'
require 'test/unit'

class TestUntitled < Test::Unit::TestCase
  def test_foo_quality_degrades_after_update
    items = [Item.new("foo", 5, 10)]
    GildedRose.new(items).update_quality()
    assert_equal 4, items[0].sell_in
    assert_equal 9, items[0].quality
  end

  def test_aged_brie_quality_increases_with_time
    items = [Item.new("Aged Brie", 5, 10)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].quality, 11
  end

  def test_sulfuras_quality_remains_constant
    items = [Item.new("Sulfuras, Hand of Ragnaros", 5, 80)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].quality, 80
  end

  def test_conjured_quality_degrades_twice_as_fast
    items = [Item.new("Conjured", 5, 10)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].quality, 6
  end

  def test_backstage_passes_quality_updates_based_on_sell_in_value
    items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 12, 10)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].quality, 11
  end

  def test_backstage_passes_quality_drops_to_zero_after_concert
    items = [Item.new("Backstage passes to a TAFKAL80ETC concert", 0, 10)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].quality, 0
  end
end
