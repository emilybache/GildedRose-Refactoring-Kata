require_relative '../../lib/gilded_rose'
require 'test/unit'

module GildedRose
  class ItemFactoryTests < Test::Unit::TestCase
    test ".create_item returns GenericItem instance if it is not a special item" do
      created_item = ItemFactory.create_item(name: "random_item", sell_in: 0, quality: 0)
      assert_instance_of GenericItem, created_item
    end

    test ".create_item returns AgedBrieItem instance if Aged Brie item is given" do
      created_item = ItemFactory.create_item(name: "Aged Brie", sell_in: 0, quality: 0)
      assert_instance_of AgedBrieItem, created_item
    end

    test ".create_item returns BackstagePassesItem instance if Backstage passes to a TAFKAL80ETC concert item is given" do
      created_item = ItemFactory.create_item(name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 0, quality: 0)
      assert_instance_of BackstagePassesItem, created_item
    end

    test ".create_item returns SulfurasItem instance if Sulfuras, Hand of Ragnaros item is given" do
      created_item = ItemFactory.create_item(name: "Sulfuras, Hand of Ragnaros", sell_in: 0, quality: 0)
      assert_instance_of SulfurasItem, created_item
    end
  end
end

