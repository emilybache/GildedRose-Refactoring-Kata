class GildedRose

  def initialize(items)
    @items = items
  end

  def legendary?(item)
    item.name == "Sulfuras, Hand of Ragnaros"
  end

  def backstage_pass?(item)
    item.name == "Backstage passes to a TAFKAL80ETC concert"
  end

  def conjured?(item)
    item.name == 'Conjured item'
  end

  def decrease_quality(item)
    return if legendary?(item)
    return if item.quality <= 0

    item.quality = item.quality - 1
  end

  def increase_quality(item)
    item.quality = item.quality + 1
  end

  def aged_brie?(item)
    item.name == "Aged Brie"
  end

  def update_item_quality(item)
    if !aged_brie?(item) and !backstage_pass?(item)
      decrease_quality(item)
      if conjured?(item)
        decrease_quality(item)
      end
    else
      if item.quality < 50
        increase_quality(item)
        if backstage_pass?(item)
          if item.sell_in < 11
            increase_quality(item)
          end
          if item.sell_in < 6
            increase_quality(item)
          end
        end
      end
    end
  end

  def update_item_sell_in(item)
    if !legendary?(item)
      item.sell_in = item.sell_in - 1
    end
  end

  def update_days_passed(item)
    if !aged_brie?(item)
      if !backstage_pass?(item)
        decrease_quality(item)
      else
        item.quality = item.quality - item.quality
      end
    else
      if item.quality < 50
        increase_quality(item)
      end
    end
  end

  def update_quality()
    @items.each do |item|
      update_item_quality(item)
      update_item_sell_in(item)
      if item.sell_in < 0
        update_days_passed(item)
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
