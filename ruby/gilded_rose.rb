# frozen_string_literal: true

class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      if item.name.include?('Backstage passes')
        item.quality += 1 if item.sell_in > 10
        item.quality += 2 if (item.sell_in <= 10) && (item.sell_in > 5)
        item.quality += 3 if item.sell_in <= 5
        item.sell_in -= 1
        item.quality = 0 if item.sell_in.negative?
      elsif !item.name.include?('Sulfuras')
        factor = 0
        factor = case item.name
                 when 'Aged Brie'
                   1
                 when /Conjured/
                   -2
                 else
                   -1
                 end
        if (item.quality >= 0) && (item.quality <= 50)
          item.sell_in -= 1
          item.quality += factor
          item.quality += factor if item.sell_in.negative?
        end
        item.quality = 50 if item.quality > 50
        item.quality = 0 if item.quality.negative?
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

  def to_s
    "#{@name}, #{@sell_in}, #{@quality}"
  end
end
