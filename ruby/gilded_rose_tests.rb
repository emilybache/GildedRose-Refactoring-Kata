require 'minitest/autorun'
require_relative 'gilded_rose'

class TestGildedRose < Minitest::Test
  def test_update_quality_regular_item
    item = Item.new('Regular Item', 5, 10)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 9, item.quality
    assert_equal 4, item.sell_in
  end

  def test_update_quality_negative_sell_in_regular_item
    item = Item.new('Regular Item', 0, 10)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 8, item.quality
  end

  def test_update_quality_aged_brie
    item = Item.new('Aged Brie', 5, 10)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 11, item.quality
  end

  def test_update_quality_max_quality_aged_brie
    item = Item.new('Aged Brie', 5, 50)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 50, item.quality
  end

  def test_update_quality_backstage_passes_approaching
    item = Item.new('Backstage passes to a TAFKAL80ETC concert', 11, 20)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 21, item.quality
  end

  def test_update_quality_backstage_passes_sell_in_10_or_less
    item = Item.new('Backstage passes to a TAFKAL80ETC concert', 10, 20)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 22, item.quality
  end

  def test_update_quality_backstage_passes_sell_in_5_or_less
    item = Item.new('Backstage passes to a TAFKAL80ETC concert', 5, 20)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 23, item.quality
  end

  def test_update_quality_backstage_passes_after_concert
    item = Item.new('Backstage passes to a TAFKAL80ETC concert', 0, 20)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 0, item.quality
  end

  def test_update_quality_sulfuras
    item = Item.new('Sulfuras, Hand of Ragnaros', 5, 80)
    gilded_rose = GildedRose.new([item])
    gilded_rose.update_quality
    assert_equal 80, item.quality
  end
end
