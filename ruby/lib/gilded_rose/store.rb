module GildedRose
  class Store

    def initialize(raw_items)
      @raw_items = raw_items
    end
  
    def update_quality()
      items.each do |item|
        if item.name != "Aged Brie" and item.name != "Backstage passes to a TAFKAL80ETC concert"
          if item.quality > 0
            if item.name != "Sulfuras, Hand of Ragnaros"
              item.quality = item.quality - 1
            end
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
        if item.name != "Sulfuras, Hand of Ragnaros"
          item.sell_in = item.sell_in - 1
        end
        if item.sell_in < 0
          if item.name != "Aged Brie"
            if item.name != "Backstage passes to a TAFKAL80ETC concert"
              if item.quality > 0
                if item.name != "Sulfuras, Hand of Ragnaros"
                  item.quality = item.quality - 1
                end
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

    def items
      @items ||= @raw_items.map do |item|
        ItemWrapperFactory.wrap(item: item)
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

  class AbstractItemWrapper
    def initialize(item: )
      @item = item
    end

    def update_quality
      if item.quality > 0
        item.quality -= 1
      end
    end

    def update_sell_in
      item.sell_in -= 1
    end

    def method_missing(method_name, *args)
      item.send(method_name, *args)
    end

    private
    attr_reader :item
  end

  class GenericItemWrapper < AbstractItemWrapper; end
  class AgedBrieItemWrapper < AbstractItemWrapper; end
  class BackstagePassesItemWrapper < AbstractItemWrapper; end
  class SulfurasItemWrapper < AbstractItemWrapper; end
end

