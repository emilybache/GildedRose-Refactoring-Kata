require File.join(File.dirname(__FILE__), 'gilded_rose')
require 'test/unit'

class ItemFactoryTests < Test::Unit::TestCase
  test ".create_item returns GenericItem instance if it is not a special item" do
    created_item = ItemFactory.create_item(name: "random_item", sell_in: 0, quality: 0)

    assert_instance_of GenericItem, created_item
  end

  test ".create_item returns Item instance if Aged Brie item if given" do
    created_item = ItemFactory.create_item(name: "Aged Brie", sell_in: 0, quality: 0)

    assert_instance_of Item, created_item
  end
end