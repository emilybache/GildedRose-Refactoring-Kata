class GildedRose
  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      case item.name
      when "Aged Brie"
        update_aged_brie(item)
      when "Backstage passes to a TAFKAL80ETC concert"
        update_backstage_passes(item)
      when "Sulfuras, Hand of Ragnaros"
        next # Sulfuras remains unchanged, move to the next item
      when "Conjured Mana Cake"
        update_conjured(item)
      else
        update_normal_item(item)
      end
    end
  end

  private

  def update_aged_brie(item)
    decrease_sell_in(item)
    increase_quality(item, 2)
  end

  def update_backstage_passes(item)
    decrease_sell_in(item)
    if item.sell_in > 0
      if item.sell_in <= 5
        increase_quality(item, 3)
      elsif item.sell_in <= 10
        increase_quality(item, 2)
      else
        increase_quality(item, 1)
      end
    else
      item.quality = 0
    end
  end

  def increase_quality(item, amount)
    item.quality = [item.quality + amount, 50].min
  end

  def update_normal_item(item)
    decrease_quality(item)
    decrease_quality(item) if expired?(item)
    decrease_sell_in(item)
  end

  def update_conjured(item)
    decrease_quality(item, 2)
    decrease_quality(item, 2) if expired?(item)
    decrease_sell_in(item)
  end

  def decrease_quality(item, rate = 1)
    item.quality -= rate if item.quality > 0
  end

  def decrease_sell_in(item)
    item.sell_in -= 1
  end

  def expired?(item)
    item.sell_in <= 0
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
