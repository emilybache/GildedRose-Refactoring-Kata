# frozen_string_literal: true

class GildedRose

  AGED_BRIE = "Aged Brie"
  BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  SULFURAS = "Sulfuras, Hand of Ragnaros"

  def initialize(items)
    @items = items
  end

  def increment_item_quality(item)
    item.quality = item.quality + 1
  end

  def first_step(item)
    if item.name == AGED_BRIE || item.name == BACKSTAGE_PASS
      return unless item.quality < 50
      increment_item_quality(item)
      return if item.name == AGED_BRIE
      if item.sell_in < 11 && item.quality < 50
        increment_item_quality(item)
      end
      if item.sell_in < 6 && item.quality < 50
        increment_item_quality(item)
      end
    else
      if item.quality > 0 && item.name != SULFURAS
        item.quality = item.quality - 1
      end
    end
  end

  def update_quality()
    @items.each do |item|
      first_step(item)
      if item.name != SULFURAS
        item.sell_in = item.sell_in - 1
      end
      if item.sell_in < 0
        if item.name == AGED_BRIE
          if item.quality < 50
            increment_item_quality(item)
          end
        else
          if item.name == BACKSTAGE_PASS
            item.quality = 0
          else
            if item.quality > 0
              if item.name != SULFURAS
                item.quality = item.quality - 1
              end
            end
          end
        end
      end
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
