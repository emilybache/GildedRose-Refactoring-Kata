class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality()
    @items.each do |item|
      case item.name
      when "Aged Brie"
        return AgedBrieItem.update_quality(item)
      when "Backstage passes to a TAFKAL80ETC concert"
        return BackstagePassItem.update_quality(item)
      when "Conjured item"
        return ConjuredItem.update_quality(item)
      when "Sulfuras, Hand of Ragnaros"
        return
      else
        return NormalItem.update_quality(item)
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

class MyItem
  def self.update_sell_in(item)
    item.sell_in = item.sell_in - 1
    return item
  end
end

class AgedBrieItem < MyItem
  def self.update_quality(item)
    item = update_sell_in(item)
    item.quality = item.quality + 1 unless item.quality >= 50
    return item
  end
end

class BackstagePassItem < MyItem
  def self.update_quality(item)
    item = update_sell_in(item)
    if item.sell_in < 0
      item.quality = 0
    elsif item.sell_in < 5
      item.quality = item.quality + 3
    elsif item.sell_in < 10
      item.quality = item.quality + 2
    else
      item.quality = item.quality + 1
    end
  end
end

class ConjuredItem < MyItem
  def self.update_quality(item)
    item = update_sell_in(item)
    if item.sell_in < 0
      item.quality = item.quality - 4
    else
      item.quality = item.quality - 2
    end
    item.quality = 0 if item.quality < 0
  end
end

class NormalItem < MyItem
  def self.update_quality(item)
    item = update_sell_in(item)
    if item.sell_in < 0
      item.quality = item.quality - 2
    else
      item.quality = item.quality - 1
    end
    item.quality = 0 if item.quality < 0
  end
end