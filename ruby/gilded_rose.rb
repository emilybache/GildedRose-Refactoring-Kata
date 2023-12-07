# frozen_string_literal: true

class GildedRose
  MAX_QUALITY = 50

  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      case item.name
      when 'Aged Brie'
        update_aged_brie(item)
      when 'Backstage passes to a TAFKAL80ETC concert'
        update_backstage_passes(item)
      when 'Sulfuras, Hand of Ragnaros'
        next # Sulfuras remains unchanged, move to the next item
      when 'Conjured Mana Cake'
        update_conjured(item)
      else
        update_normal_item(item)
      end
    end
  end

  private

  def update_aged_brie(item)
    increase_quality(item)
    decrease_sell_in(item)
  end

  def update_backstage_passes(item)
    if positive_sell?(item)
      if item.sell_in > 11
        increase_quality(item)
      elsif item.sell_in > 6
        increase_quality(item, 2)
      else
        increase_quality(item, 3)
      end
    else
      item.quality = 0
    end

    decrease_sell_in(item)
  end

  def update_normal_item(item)
    decrease_quality(item, 2)
    decrease_sell_in(item)
  end

  def update_conjured(item)
    decrease_quality(item, 2)
    decrease_sell_in(item)
  end

  def increase_quality(item, rate = 1)
    return item.quality if minimum_sell?(item)

    item.quality += rate if maximum_quality?(item)
  end

  def decrease_sell_in(item)
    item.sell_in -= 1
  end

  def decrease_quality(item, rate = 1)
    return item.quality if minimum_sell?(item)

    item.quality -= rate if positive_sell?(item)
  end

  def positive_sell?(item)
    item.sell_in.positive?
  end

  def negative_sell?(item)
    item.sell_in.negative?
  end

  def minimum_sell?(item)
    item.sell_in <= 0
  end

  def maximum_quality?(item)
    item.quality < MAX_QUALITY
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
