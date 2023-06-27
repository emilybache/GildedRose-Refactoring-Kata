class GildedRose

  def initialize(items)
    @items = items
  end

  def update_quality
    @items.each do |item|
      case item.name
      when 'Sulfuras, Hand of Ragnaros'
        update_sulfuras_quality(item)
      else
        if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert"
          if item.quality > 0
            item.quality = item.quality - 1
          end
        else
          if item.quality < 50
            item.quality = item.quality + 1
            if item.name == "Backstage passes to a TAFKAL80ETC concert"
              if item.sell_in < 11
                if item.quality < 50
                  item.quality = item.quality + 1
                end
              end
              if item.sell_in < 6
                if item.quality < 50
                  item.quality = item.quality + 1
                end
              end
            end
          end
        end
        item.sell_in = item.sell_in - 1
        if item.sell_in < 0
          if item.name != "Aged Brie"
            if item.name != "Backstage passes to a TAFKAL80ETC concert"
              if item.quality > 0
                item.quality = item.quality - 1
              end
            else
              item.quality = item.quality - item.quality
            end
          else
            if item.quality < 50
              item.quality = item.quality + 1
            end
          end
        end
      end
    end
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
