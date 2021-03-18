module GildedRose
  class Store

    def initialize(raw_items)
      @raw_items = raw_items
    end
  
    def update_quality()
      items.each do |item|
        item.update_quality
        item.update_sell_in
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
      if item.sell_in <= 0 && item.quality > 0
        item.quality -= 2
      elsif item.quality > 0
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
  class AgedBrieItemWrapper < AbstractItemWrapper;
    def update_quality
      if item.quality < 50
        item.quality = item.quality + 1
      end
    end
  end

  class BackstagePassesItemWrapper < AbstractItemWrapper;
    # if sell_in is 0, then quality becomes 0
    # if quality is already 50, then quality stays at 50
    # if sell in is 5 or below, quality increases by 3
    # if sell in is 10 or below, quality increases by 2
    # if sell in is 10 or greater, quality increases by 1
    def update_quality
      if item.sell_in == 0
        item.quality = 0
      elsif reached_max_quality?
        # noop
      elsif item.sell_in < 6
        item.quality += 3
      elsif item.sell_in < 11
        item.quality += 2
      else
        item.quality += 1
      end
    end

    def reached_max_quality?
      item.quality >= 50
    end
  end

  class SulfurasItemWrapper < AbstractItemWrapper;
    def update_quality
    end
    def update_sell_in
    end
  end
end

