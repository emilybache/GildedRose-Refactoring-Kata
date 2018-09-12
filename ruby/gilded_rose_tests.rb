require File.join(File.dirname(__FILE__), 'gilded_rose')
require 'test/unit'

class GildedRoseTest < Test::Unit::TestCase

  def test_update_plus_5_dexterity_vest_once
    items = [Item.new(name="+5 Dexterity Vest", sell_in=10, quality=20)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 9
    assert_equal items[0].quality, 19
  end

  def test_update_plus_5_dexterity_vest_twice
    items = [Item.new(name="+5 Dexterity Vest", sell_in=10, quality=20)]
    2.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 8
    assert_equal items[0].quality, 18
  end

  def test_update_plus_5_dexterity_vest_thrice
    items = [Item.new(name="+5 Dexterity Vest", sell_in=10, quality=20)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 7
    assert_equal items[0].quality, 17
  end

  def test_update_plus_5_dexterity_vest_four_times
    items = [Item.new(name="+5 Dexterity Vest", sell_in=10, quality=20)]
    4.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 6
    assert_equal items[0].quality, 16
  end

  def test_update_aged_brie_once
    items = [Item.new(name="Aged Brie", sell_in=2, quality=0)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 1
    assert_equal items[0].quality, 1
  end

  def test_update_aged_brie_twice
    items = [Item.new(name="Aged Brie", sell_in=2, quality=0)]
    2.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 0
    assert_equal items[0].quality, 2
  end

  def test_update_aged_brie_thrice
    items = [Item.new(name="Aged Brie", sell_in=2, quality=0)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -1
    assert_equal items[0].quality, 4
  end

  def test_update_aged_brie_four_times
    items = [Item.new(name="Aged Brie", sell_in=2, quality=0)]
    4.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -2
    assert_equal items[0].quality, 6
  end

  def test_elixir_of_the_mongoose_once
    items = [Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 4
    assert_equal items[0].quality, 6
  end

  def test_elixir_of_the_mongoose_twice
    items = [Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7)]
    2.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 3
    assert_equal items[0].quality, 5
  end

  def test_elixir_of_the_mongoose_thrice
    items = [Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 2
    assert_equal items[0].quality, 4
  end

  def test_elixir_of_the_mongoose_four_times
    items = [Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7)]
    4.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 1
    assert_equal items[0].quality, 3
  end

  def test_elixir_of_the_mongoose_six_times
    items = [Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7)]
    6.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -1
    assert_equal items[0].quality, 0
  end

  def test_elixir_of_the_mongoose_seven_times
    items = [Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7)]
    7.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -2
    assert_equal items[0].quality, 0
  end

  def test_sulfuras_hand_of_ragnaros_once
    items = [Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 0
    assert_equal items[0].quality, 80
  end

  def test_sulfuras_hand_of_ragnaros_twice
    items = [Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=0, quality=80)]
    2.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 0
    assert_equal items[0].quality, 80
  end

  def test_sulfuras_hand_of_ragnaros_thrice
    items = [Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=-1, quality=80)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -1
    assert_equal items[0].quality, 80
  end

  def test_sulfuras_hand_of_ragnaros_six_times
    items = [Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=-1, quality=80)]
    6.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -1
    assert_equal items[0].quality, 80
  end

  def test_backstage_passes_to_concert_once
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 14
    assert_equal items[0].quality, 21
  end

  def test_backstage_passes_to_concert_thrice
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 12
    assert_equal items[0].quality, 23
  end

  def test_backstage_passes_to_concert_six_times
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20)]
    6.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 9
    assert_equal items[0].quality, 27
  end

  def test_backstage_passes_to_concert_lower_sellin_once
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=49)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 9
    assert_equal items[0].quality, 50
  end

  def test_backstage_passes_to_concert_lower_sell_in_thrice
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=40)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 7
    assert_equal items[0].quality, 46
  end

  def test_backstage_passes_to_concert_lower_sell_in_six_times
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=49)]
    6.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 4
    assert_equal items[0].quality, 50
  end

  def test_backstage_passes_to_concert_lowest_sellin_once
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 4
    assert_equal items[0].quality, 50
  end

  def test_backstage_passes_to_concert_lowest_sell_in_thrice
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=40)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 2
    assert_equal items[0].quality, 49
  end

  def test_backstage_passes_to_concert_lowest_sell_in_six_times
    items = [Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49)]
    6.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -1
    assert_equal items[0].quality, 0
  end

  def test_conjure_once
    items = [Item.new(name="Conjured Mana Cake", sell_in=3, quality=6)]
    GildedRose.new(items).update_quality()
    assert_equal items[0].sell_in, 2
    assert_equal items[0].quality, 5
  end

  def test_conjure_thrice
    items = [Item.new(name="Conjured Mana Cake", sell_in=3, quality=6)]
    3.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, 0
    assert_equal items[0].quality, 3
  end

  def test_conjure_four_times
    items = [Item.new(name="Conjured Mana Cake", sell_in=3, quality=6)]
    4.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -1
    assert_equal items[0].quality, 1
  end

  def test_conjure_six_times
    items = [Item.new(name="Conjured Mana Cake", sell_in=3, quality=6)]
    6.times { GildedRose.new(items).update_quality() }
    assert_equal items[0].sell_in, -3
    assert_equal items[0].quality, 0
  end
end