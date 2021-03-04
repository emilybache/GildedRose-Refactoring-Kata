require_relative '../../lib/gilded_rose'
require 'test/unit'

module GildedRose
  class ItemWrapperFactoryTests < Test::Unit::TestCase
    test ".create_item returns GenericItem instance if it is not a special item" do
      item = make_item(name: "random_item", sell_in: 0, quality: 0)

      created_item = ItemWrapperFactory.wrap(item: item)
      assert_instance_of GenericItemWrapper, created_item
    end

    test ".create_item returns AgedBrieItem instance if Aged Brie item is given" do
      item = make_item(name: "Aged Brie", sell_in: 0, quality: 0)

      created_item = ItemWrapperFactory.wrap(item: item)
      assert_instance_of AgedBrieItemWrapper, created_item
    end

    test ".create_item returns BackstagePassesItem instance if Backstage passes to a TAFKAL80ETC concert item is given" do
      item = make_item(name: "Backstage passes to a TAFKAL80ETC concert", sell_in: 0, quality: 0)

      created_item = ItemWrapperFactory.wrap(item: item)
      assert_instance_of BackstagePassesItemWrapper, created_item
    end

    test ".create_item returns SulfurasItem instance if Sulfuras, Hand of Ragnaros item is given" do
      item = make_item(name: "Sulfuras, Hand of Ragnaros", sell_in: 0, quality: 0)

      created_item = ItemWrapperFactory.wrap(item: item)
      assert_instance_of SulfurasItemWrapper, created_item
    end


    private

    def make_item(name:, sell_in:, quality:)
      Item.new(name, sell_in, quality)
    end
  end
end

