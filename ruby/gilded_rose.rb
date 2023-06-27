class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      case item.name
      when 'Aged Brie'
        update_aged_brie_quality(item)
      when 'Backstage passes to a TAFKAL80ETC concert'
        update_backstage_passes_quality(item)
      when 'Sulfuras, Hand of Ragnaros'
        update_sulfuras_quality(item)
      else
        update_normal_quality(item)
      end
    end
  end

  def update_aged_brie_quality(item)
    item.sell_in -= 1

    item.quality += 1
    item.quality += 1 if item.sell_in.negative?
    item.quality = 50 if item.quality > 50
  end

  def update_backstage_passes_quality(item)
    item.sell_in -= 1
    return item.quality = 0 if item.sell_in.negative?

    item.quality += 1
    item.quality += 1 if item.sell_in < 10
    item.quality += 1 if item.sell_in < 5
    item.quality = 50 if item.quality > 50
  end

  def update_normal_quality(item)
    item.sell_in -= 1
    item.quality -= 1
    item.quality -= 1 if item.sell_in.negative?
    item.quality = 0 if item.quality.negative?
  end

  def update_sulfuras_quality(item); end
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
