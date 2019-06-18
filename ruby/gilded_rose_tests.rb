require File.join(File.dirname(__FILE__), 'gilded_rose')
require 'test/unit'

class TestUntitled < Test::Unit::TestCase

  def test_gilded_rose
    
    items = [
       Item.new(name="+5 Dexterity Vest", sell_in=10, quality=20),
       Item.new(name="Aged Brie", sell_in=2, quality=0),
       Item.new(name="Elixir of the Mongoose", sell_in=5, quality=7),
       Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=0, quality=80),
       Item.new(name="Sulfuras, Hand of Ragnaros", sell_in=-1, quality=80),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=15, quality=20),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=10, quality=49),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=49),
       Item.new(name="Backstage passes to a TAFKAL80ETC concert", sell_in=5, quality=39),
       # Now it works perfectly
       Item.new(name="Conjured Mana Cake", sell_in=0, quality=6),
       Item.new(name="Conjured Mana Cake", sell_in=3, quality=6),
    ]
    
      GildedRose.update_quality()
      assert_equal items[0].quality, 19
      assert_equal items[1].quality, 1
      assert_equal items[2].quality, 6
      assert_equal items[3].quality, 80
      assert_equal items[4].quality, 80
      assert_equal items[5].quality, 21
      assert_equal items[6].quality, 50
      assert_equal items[7].quality, 50
      assert_equal items[8].quality, 42
      assert_equal items[9].quality, 2
      assert_equal items[10].quality, 4
  end

end