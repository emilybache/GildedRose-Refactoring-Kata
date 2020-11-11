require File.join(File.dirname(__FILE__), 'gilded_rose')
require 'test/unit'

class TestGildedRoseRefactor < Test::Unit::TestCase
  setup do
    @items = [
      Item.new(name: '+5 Dexterity Vest', sell_in: 10, quality: 20),
      Item.new(name: 'Aged Brie', sell_in: 2, quality: 0),
      Item.new(name: 'Elixir of the Mongoose', sell_in: 5, quality: 7),
      Item.new(name: 'Sulfuras, Hand of Ragnaros', sell_in: 0, quality: 80),
      Item.new(name: 'Sulfuras, Hand of Ragnaros', sell_in: -1, quality: 80),
      Item.new(name: 'Backstage passes to a TAFKAL80ETC concert', sell_in: 15, quality: 20),
      Item.new(name: 'Backstage passes to a TAFKAL80ETC concert', sell_in: 10, quality: 49),
      Item.new(name: 'Backstage passes to a TAFKAL80ETC concert', sell_in: 5, quality: 49),
      Item.new(name: 'Conjured Mana Cake', sell_in: 3, quality: 12)
    ]
  end

  def test_foo
    gilded_rose = GildedRose.new @items
    string = "OMGHAI!\n".force_encoding('UTF-8')
    (0...31).each do |day|
      string << "-------- day #{day} --------\n"
      string << "name, sellIn, quality\n"
      @items.each { |item| string << item.to_s + "\n" }
      string << "\n"
      gilded_rose.update_quality
    end
    assert_equal File.read('test_text_fixture.txt').force_encoding('UTF-8'), string.force_encoding('UTF-8')
  end
end
