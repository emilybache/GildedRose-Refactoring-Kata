class GildedRose
  QUALITY_LOWER_LIMIT = 0
  QUALITY_UPPER_LIMIT = 50

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
      when 'Conjured Mana Cake'
        update_conjured_quality(item)
      when 'Sulfuras, Hand of Ragnaros'
        update_sulfuras_quality(item)
      else
        update_normal_quality(item)
      end
    end
  end

  def update_aged_brie_quality(item)
    decrease_sell_in_day(item)

    increase_item_quality(item)
    increase_item_quality(item) if item.sell_in.negative?

    keep_quality_upper_limit_in_bounds(item)
  end

  def update_backstage_passes_quality(item)
    decrease_sell_in_day(item)
    return item.quality = 0 if item.sell_in.negative?

    increase_item_quality(item)
    increase_item_quality(item) if item.sell_in < 10
    increase_item_quality(item) if item.sell_in < 5

    keep_quality_upper_limit_in_bounds(item)
  end

  def update_conjured_quality(item)
    decrease_sell_in_day(item)

    decrease_item_quality(item, 2)
    decrease_item_quality(item, 2) if item.sell_in.negative?

    keep_quality_lower_limit_in_bounds(item)
  end

  def update_normal_quality(item)
    decrease_sell_in_day(item)

    decrease_item_quality(item)
    decrease_item_quality(item) if item.sell_in.negative?

    keep_quality_lower_limit_in_bounds(item)
  end

  def update_sulfuras_quality(item); end

  private

    def decrease_sell_in_day(item)
      item.sell_in -= 1
    end

    def increase_item_quality(item)
      item.quality += 1
    end

    def decrease_item_quality(item, by = 1)
      item.quality -= by
    end

    def keep_quality_upper_limit_in_bounds(item)
      item.quality = QUALITY_UPPER_LIMIT if item.quality > QUALITY_UPPER_LIMIT
    end

    def keep_quality_lower_limit_in_bounds(item)
      item.quality = QUALITY_LOWER_LIMIT if item.quality < QUALITY_LOWER_LIMIT
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
