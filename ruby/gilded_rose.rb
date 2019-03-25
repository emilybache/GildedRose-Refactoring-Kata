class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality()
    @items.each do |item|
      case item.name
      when "Aged Brie"
        item.sell_in = item.sell_in - 1
        item.quality = item.quality + 1 unless item.quality >= 50
        return
      when "Backstage passes to a TAFKAL80ETC concert"
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0
          item.quality = 0
        elsif item.sell_in < 5
          item.quality = item.quality + 3
        elsif item.sell_in < 10
          item.quality = item.quality + 2
        else
          item.quality = item.quality + 1
        end
        return
      when "Conjured item"
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0
          item.quality = item.quality - 4
        else
          item.quality = item.quality - 2
        end
        item.quality = 0 if item.quality < 0
        return
      when "Sulfuras, Hand of Ragnaros"
        return
      else
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0
          item.quality = item.quality - 2
        else
          item.quality = item.quality - 1
        end
        item.quality = 0 if item.quality < 0
        return
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