class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality()
    @items.each do |item|
      next if item.name == "Sulfuras, Hand of Ragnaros"

      if item.name == "Aged Brie"
        increases_proportionally_to_times(item)
      elsif item.name == "Backstage passes to a TAFKAL80ETC concert"
        increases_proportionally_to_times(item)
      else
        decrease_quality(item)
      end

      item.sell_in -= 1 if item.name != "Sulfuras, Hand of Ragnaros"

      if item.sell_in < 0
        if item.name == "Aged Brie"
          item.quality += 1 if item.quality < 50
        elsif item.name == "Backstage passes to a TAFKAL80ETC concert"
          item.quality = 0
        else
          decrease_quality(item)
        end
      end
    end
  end

  private

  def decrease_quality(item)
    decrement = item.name == "Conjured" ? 2 : 1
    item.quality -= decrement if item.quality > 0
  end

  def increases_proportionally_to_times(item)
    if item.sell_in <= 10
      item.quality += 2 if item.quality < 50
    end
    if item.sell_in <= 5
      item.quality += 3 if item.quality < 50
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
