# frozen_string_literal: true

class GildedRose

  AGED_BRIE = "Aged Brie"
  BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  SULFURAS = "Sulfuras, Hand of Ragnaros"
  QUALITY_THRESHOLD = 50

  def initialize(items)
    @items = items
  end

  def increment_item_quality(item, num = 1)
    item.quality = [item.quality + num, QUALITY_THRESHOLD].min
  end

  def first_step(item)
    case item.name
    when AGED_BRIE
      increment_item_quality(item)
    when BACKSTAGE_PASS
      if item.sell_in >= 11
        increment_item_quality(item)
      elsif (6...11).cover?(item.sell_in)
        increment_item_quality(item, 2)
      elsif item.sell_in < 6
        increment_item_quality(item, 3)
      end
    else
      if item.quality > 0
        item.quality = item.quality - 1
      end
    end
  end

  def update_quality()
    @items.each do |item|
      return if item.name == SULFURAS

      first_step(item)
      item.sell_in = item.sell_in - 1
      if item.sell_in < 0
        if item.name == AGED_BRIE
          increment_item_quality(item)
        else
          if item.name == BACKSTAGE_PASS
            item.quality = 0
          else
            if item.quality > 0
              item.quality = item.quality - 1
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
