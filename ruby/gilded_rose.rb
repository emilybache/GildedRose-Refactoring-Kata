class GildedRose

  def initialize(items)
    @items = items
  end

  def not_legendary?(item)
    item.name != "Sulfuras, Hand of Ragnaros"
  end

  def backstage_pass?(item)
    item.name == "Backstage passes to a TAFKAL80ETC concert"
  end

  def decrease_quality(item)
    item.quality = item.quality - 1
  end

  def increase_quality(item)
    item.quality = item.quality + 1
  end

  def update_quality()
    @items.each do |item|
      if item.name != "Aged Brie" and !backstage_pass?(item)
        if item.quality > 0
          if not_legendary?(item)
            decrease_quality(item)
          end
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
      if not_legendary?(item)
        item.sell_in = item.sell_in - 1
      end
      if item.sell_in < 0
        if item.name != "Aged Brie"
          if !backstage_pass?(item)
            if item.quality > 0
              if not_legendary?(item)
                decrease_quality(item)
              end
            end
          else
            item.quality = item.quality - item.quality
          end
        else
          if item.quality < 50
            increase_quality(item)
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
