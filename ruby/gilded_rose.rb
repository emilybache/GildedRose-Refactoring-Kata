require_relative 'items/backstage_pass_item'
require_relative 'items/brie_item'
require_relative 'items/conjured_item'
require_relative 'items/normal_item'
require_relative 'items/sulfura_item'

class GildedRose
  QUALITY_LOWER_LIMIT = 0
  QUALITY_UPPER_LIMIT = 50

  SPECIALITY_ITEM_MAPPER = {
    'Aged Brie' => BrieItem,
    'Backstage passes to a TAFKAL80ETC concert' => BackstagePassItem,
    'Conjured Mana Cake' => ConjuredItem,
    'Sulfuras, Hand of Ragnaros' => SulfuraItem
  }.freeze

  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      (SPECIALITY_ITEM_MAPPER[item.name] || NormalItem).new(item).spend_day_in_shop
    end
  end
end

class Item
  attr_accessor :name, :sell_in, :quality

  def initialize(name, sell_in, quality)
    @name = name
    @sell_in = sell_in
    @quality = quality
  end

  def to_s()
    "#{@name}, #{@sell_in}, #{@quality}"
  end
end
